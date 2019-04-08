declare namespace isc {
  /**
   * Possible values for Canvas.backgroundRepeat.
   */
  type BackgroundRepeat =
    /**
     * Tile the background image horizontally and vertically.
     */
    "repeat" |

    /**
     * Don't tile the background image at all.
     */
    "no-repeat" |

    /**
     * Repeat the background image horizontally but not vertically.
     */
    "repeat-x" |

    /**
     * Repeat the background image vertically but not horizontally.
     */
    "repeat-y";

  type SomeType = A | B | C

  class Canvas extends BaseWidget {
    /**
     * Specifies how the background image should be tiled if this widget
     *  is larger than the image. It corresponds to the CSS background-repeat
     *  attribute.
     *
     *  The default of null means no background-repeat CSS will be
     *  written out. See BackgroundRepeat type for details on other settings.
     *
     *  NOTE: this setting directly sets the CSS property background-repeat but
     *  does not attempt to work around various known bugs with this setting, or lack of support
     *  in IE6. If you need to apply CSS-based workarounds for browser limitations with
     *  this setting, it's best to do so via setting Canvas.styleName.
     * @type {BackgroundRepeat}
     * @default null
     */
    backgroundRepeat: BackgroundRepeat;

  }

}