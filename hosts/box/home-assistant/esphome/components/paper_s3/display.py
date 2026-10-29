import esphome.codegen as cg
from esphome.components import display
from esphome.components.esp32 import include_builtin_idf_component
import esphome.config_validation as cv
from esphome.const import CONF_ID, CONF_LAMBDA

DEPENDENCIES = ["esp32", "psram"]

CONF_FAST_PHASES = "fast_phases"

paper_s3_ns = cg.esphome_ns.namespace("paper_s3")
PaperS3Display = paper_s3_ns.class_("PaperS3Display", display.DisplayBuffer)

CONFIG_SCHEMA = (
    display.FULL_DISPLAY_SCHEMA.extend(
        {
            cv.GenerateID(): cv.declare_id(PaperS3Display),
            # Drive phases of the fast black/white waveform used by `refresh()`;
            # fewer is faster but leaves blacks lighter.
            cv.Optional(CONF_FAST_PHASES, default=8): cv.int_range(min=1, max=30),
        }
    )
    .extend(cv.polling_component_schema("never"))
)


async def to_code(config):
    var = cg.new_Pvariable(config[CONF_ID])
    await display.register_display(var, config)
    cg.add(var.set_fast_phases(config[CONF_FAST_PHASES]))

    # epdiy's board support references the ADC driver.
    include_builtin_idf_component("esp_adc")

    if CONF_LAMBDA in config:
        lambda_ = await cg.process_lambda(
            config[CONF_LAMBDA], [(display.DisplayRef, "it")], return_type=cg.void
        )
        cg.add(var.set_writer(lambda_))
