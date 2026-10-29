#include "paper_s3.h"

#include <algorithm>
#include <cstring>

#include <driver/gpio.h>
#include <esp_rom_sys.h>

#include "esphome/core/log.h"

extern "C" {
#include "epd_board.h"
#include "output_lcd/lcd_driver.h"
}

namespace esphome::paper_s3 {

static const char *const TAG = "paper_s3";

static constexpr gpio_num_t PIN_PWR = GPIO_NUM_45;
static constexpr gpio_num_t PIN_BST_EN = GPIO_NUM_46;
static constexpr gpio_num_t PIN_XSTL = GPIO_NUM_13;
static constexpr gpio_num_t PIN_XLE = GPIO_NUM_15;
static constexpr gpio_num_t PIN_SPV = GPIO_NUM_17;
static constexpr gpio_num_t PIN_CKV = GPIO_NUM_18;
static constexpr gpio_num_t PIN_PCLK = GPIO_NUM_16;
static constexpr gpio_num_t PIN_DATA[8] = {GPIO_NUM_6, GPIO_NUM_14, GPIO_NUM_7, GPIO_NUM_12,
                                           GPIO_NUM_9, GPIO_NUM_11, GPIO_NUM_8, GPIO_NUM_10};
static constexpr gpio_num_t CONTROL_PINS[] = {PIN_PWR, PIN_BST_EN, PIN_XSTL, PIN_XLE, PIN_SPV, PIN_CKV};

// The panel has no temperature sensor.
static constexpr int TEMPERATURE = 25;

static void board_init(uint32_t, const EpdInitConfig *) {
  for (gpio_num_t pin : CONTROL_PINS) {
    gpio_set_direction(pin, GPIO_MODE_OUTPUT);
    gpio_set_level(pin, 0);
  }

  const EpdDisplay_t *display = epd_get_display();
  LcdEpdConfig_t config{};
  config.pixel_clock = display->bus_speed * 1000 * 1000;
  config.ckv_high_time = 70;
  config.line_front_porch = 4;
  config.le_high_time = 4;
  config.bus_width = 8;
  for (auto &pin : config.bus.data)
    pin = GPIO_NUM_NC;
  for (int i = 0; i < 8; i++)
    config.bus.data[i] = PIN_DATA[i];
  config.bus.clock = PIN_PCLK;
  config.bus.ckv = PIN_CKV;
  config.bus.start_pulse = PIN_XSTL;
  config.bus.leh = PIN_XLE;
  config.bus.stv = PIN_SPV;
  epd_lcd_init(&config, display->width, display->height);
}

static void board_deinit() {
  epd_lcd_deinit();
  gpio_set_level(PIN_PWR, 0);
  gpio_set_level(PIN_BST_EN, 0);
}

static void board_set_ctrl(epd_ctrl_state_t *state, const epd_ctrl_state_t *const mask) {
  if (mask->ep_sth)
    gpio_set_level(PIN_XSTL, state->ep_sth);
  if (mask->ep_stv)
    gpio_set_level(PIN_SPV, state->ep_stv);
  if (mask->ep_latch_enable)
    gpio_set_level(PIN_XLE, state->ep_latch_enable);
}

static void board_poweron(epd_ctrl_state_t *) {
  gpio_set_level(PIN_PWR, 1);
  esp_rom_delay_us(200);
  gpio_set_level(PIN_BST_EN, 1);
  esp_rom_delay_us(200);
  gpio_set_level(PIN_SPV, 1);
  gpio_set_level(PIN_XSTL, 1);
}

static void board_poweroff(epd_ctrl_state_t *) {
  gpio_set_level(PIN_BST_EN, 0);
  esp_rom_delay_us(50);
  gpio_set_level(PIN_PWR, 0);
  esp_rom_delay_us(200);
  for (gpio_num_t pin : {PIN_SPV, PIN_XSTL, PIN_XLE, PIN_CKV})
    gpio_set_level(pin, 0);
  for (gpio_num_t pin : PIN_DATA)
    gpio_set_level(pin, 0);
}

static float board_get_temperature() { return TEMPERATURE; }

// epdiy's 4-bit grey level, from black (0) to white (15).
static uint8_t to_grey(Color color) { return ((color.r + color.g + color.b) / 3) >> 4; }

static const EpdBoardDefinition BOARD = {
    .init = board_init,
    .deinit = board_deinit,
    .set_ctrl = board_set_ctrl,
    .poweron = board_poweron,
    .measure_vcom = nullptr,
    .poweroff = board_poweroff,
    .set_vcom = nullptr,
    .get_temperature = board_get_temperature,
    .gpio_set_direction = nullptr,
    .gpio_read = nullptr,
    .gpio_write = nullptr,
};

// The built-in waveform plus a fast black/white mode, registered as MODE_A2.
// It pushes every changed pixel toward its target for each phase, at every
// temperature.
static constexpr int MAX_PHASES = 30;
static constexpr int MAX_MODES = 8;
static constexpr int MAX_TEMP_RANGES = 16;
static uint8_t fast_luts[MAX_PHASES][16][4];
static EpdWaveformPhases fast_phases;
static const EpdWaveformPhases *fast_ranges[MAX_TEMP_RANGES];
static EpdWaveformMode fast_mode;
static const EpdWaveformMode *modes[MAX_MODES];
static EpdWaveform waveform;

static const EpdWaveform *build_waveform(int phases) {
  // Two bits per (target, source) pair: 1 darkens, 2 lightens.
  std::memset(fast_luts, 0, sizeof(fast_luts));
  for (int phase = 0; phase < phases; phase++) {
    for (int to = 0; to < 16; to++) {
      for (int from = 0; from < 16; from++) {
        uint8_t action = from > to ? 1 : from < to ? 2 : 0;
        fast_luts[phase][to][from / 4] |= action << (6 - 2 * (from % 4));
      }
    }
  }
  fast_phases.phases = phases;
  fast_phases.luts = &fast_luts[0][0][0];
  fast_phases.phase_times = nullptr;

  const EpdWaveform *builtin = epd_get_display()->default_waveform;
  for (int i = 0; i < builtin->num_temp_ranges; i++)
    fast_ranges[i] = &fast_phases;
  fast_mode.type = MODE_A2;
  fast_mode.temp_ranges = builtin->num_temp_ranges;
  fast_mode.range_data = fast_ranges;

  for (int i = 0; i < builtin->num_modes; i++)
    modes[i] = builtin->mode_data[i];
  modes[builtin->num_modes] = &fast_mode;

  waveform.num_modes = builtin->num_modes + 1;
  waveform.num_temp_ranges = builtin->num_temp_ranges;
  waveform.mode_data = modes;
  waveform.temp_intervals = builtin->temp_intervals;
  return &waveform;
}

void PaperS3Display::setup() {
  epd_init(&BOARD, &ED047TC2, EPD_LUT_64K);
  this->hl_ = epd_hl_init(build_waveform(this->fast_phases_));
  this->framebuffer_ = epd_hl_get_framebuffer(&this->hl_);

  epd_poweron();
  epd_fullclear(&this->hl_, TEMPERATURE);
  epd_poweroff();
}

void PaperS3Display::dump_config() {
  LOG_DISPLAY("", "PaperS3 Display", this);
  ESP_LOGCONFIG(TAG, "  Fast phases: %d", this->fast_phases_);
}

void PaperS3Display::update() {
  epd_poweron();
  epd_fullclear(&this->hl_, TEMPERATURE);
  this->do_update_();
  epd_hl_update_screen(&this->hl_, MODE_GC16, TEMPERATURE);
  epd_poweroff();
}

void PaperS3Display::refresh(int x, int y, int w, int h) {
  this->do_update_();
  epd_poweron();
  EpdDrawError err = epd_hl_update_area(&this->hl_, MODE_A2, TEMPERATURE, this->to_panel_(x, y, w, h));
  epd_poweroff();
  if (err != EPD_DRAW_SUCCESS)
    ESP_LOGE(TAG, "Refresh failed: %d", err);
}

void HOT PaperS3Display::fill_rect(int x, int y, int w, int h, Color color) {
  int x0 = std::max(x, 0), y0 = std::max(y, 0);
  int x1 = std::min(x + w, this->get_width()), y1 = std::min(y + h, this->get_height());
  if (x0 >= x1 || y0 >= y1)
    return;
  EpdRect area = this->to_panel_(x0, y0, x1 - x0, y1 - y0);
  uint8_t grey = to_grey(color);
  for (int row = area.y; row < area.y + area.height; row++) {
    uint8_t *line = &this->framebuffer_[row * this->get_width_internal() / 2];
    int start = area.x, end = area.x + area.width;
    if (start % 2) {
      line[start / 2] = (line[start / 2] & 0x0F) | (grey << 4);
      start++;
    }
    if (end % 2) {
      line[end / 2] = (line[end / 2] & 0xF0) | grey;
      end--;
    }
    if (start < end)
      std::memset(&line[start / 2], grey * 0x11, (end - start) / 2);
  }
}

EpdRect PaperS3Display::to_panel_(int x, int y, int w, int h) {
  switch (this->rotation_) {
    case display::DISPLAY_ROTATION_90_DEGREES:
      return {.x = this->get_width_internal() - y - h, .y = x, .width = h, .height = w};
    case display::DISPLAY_ROTATION_180_DEGREES:
      return {.x = this->get_width_internal() - x - w,
              .y = this->get_height_internal() - y - h,
              .width = w,
              .height = h};
    case display::DISPLAY_ROTATION_270_DEGREES:
      return {.x = y, .y = this->get_height_internal() - x - w, .width = h, .height = w};
    default:
      return {.x = x, .y = y, .width = w, .height = h};
  }
}

// Unlike DisplayBuffer's, this skips feeding the watchdog on every pixel.
void HOT PaperS3Display::draw_pixel_at(int x, int y, Color color) {
  if (!this->get_clipping().inside(x, y))
    return;
  switch (this->rotation_) {
    case display::DISPLAY_ROTATION_0_DEGREES:
      break;
    case display::DISPLAY_ROTATION_90_DEGREES:
      std::swap(x, y);
      x = this->get_width_internal() - x - 1;
      break;
    case display::DISPLAY_ROTATION_180_DEGREES:
      x = this->get_width_internal() - x - 1;
      y = this->get_height_internal() - y - 1;
      break;
    case display::DISPLAY_ROTATION_270_DEGREES:
      std::swap(x, y);
      y = this->get_height_internal() - y - 1;
      break;
  }
  this->draw_absolute_pixel_internal(x, y, color);
}

// The framebuffer holds two 4-bit grey levels per byte, the even pixel in the
// low nibble.
void HOT PaperS3Display::draw_absolute_pixel_internal(int x, int y, Color color) {
  if (x < 0 || x >= this->get_width_internal() || y < 0 || y >= this->get_height_internal())
    return;
  uint8_t grey = to_grey(color);
  uint8_t *byte = &this->framebuffer_[y * this->get_width_internal() / 2 + x / 2];
  if (x % 2)
    *byte = (*byte & 0x0F) | (grey << 4);
  else
    *byte = (*byte & 0xF0) | grey;
}

}  // namespace esphome::paper_s3
