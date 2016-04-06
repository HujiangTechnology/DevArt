package com.hujiang.devart.component.flip

/**
 * Created by rarnu on 4/6/16.
 */
object OverFlipperFactory {

    fun create(v: FlipView?, mode: OverFlipMode): OverFlipper? =
            when (mode) {
                OverFlipMode.GLOW -> GlowOverFlipper(v)
                OverFlipMode.RUBBER_BAND -> RubberBandOverFlipper()
                else -> null
            }
}