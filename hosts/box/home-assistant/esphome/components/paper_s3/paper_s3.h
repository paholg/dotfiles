#pragma once

#include "esphome/components/display/display_buffer.h"

#include "epdiy.h"

namespace esphome::paper_s3 {

// M5Stack PaperS3 e-paper panel, driven by epdiy.
//
// Drawing goes straight into epdiy's framebuffer; nothing reaches the panel
// until `refresh()` or `update()`.
class PaperS3Display : public display::DisplayBuffer {
 public:
  void setup() override;
  void dump_config() override;
  float get_setup_priority() const override { return setup_priority::PROCESSOR; }

  // Redraws everything with the full greyscale waveform, clearing ghosting.
  void update() override;
  // Redraws and refreshes a rectangle, in rotated coordinates, with the fast
  // black/white waveform.
  void refresh(int x, int y, int w, int h);
  // Fills a rectangle, in rotated coordinates, a panel row at a time; much
  // faster than `filled_rectangle()`, which goes pixel by pixel.
  void fill_rect(int x, int y, int w, int h, Color color);

  void set_fast_phases(int phases) { this->fast_phases_ = phases; }

  void draw_pixel_at(int x, int y, Color color) override;
  display::DisplayType get_display_type() override { return display::DisplayType::DISPLAY_TYPE_GRAYSCALE; }

 protected:
  void draw_absolute_pixel_internal(int x, int y, Color color) override;
  EpdRect to_panel_(int x, int y, int w, int h);
  int get_width_internal() override { return 960; }
  int get_height_internal() override { return 540; }

  int fast_phases_;
  EpdiyHighlevelState hl_;
  uint8_t *framebuffer_{nullptr};
};

}  // namespace esphome::paper_s3
