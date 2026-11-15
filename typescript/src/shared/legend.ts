/**
 * The legend members of a config. rtemis.draw's widget and SVG export place
 * the legend from them after the option is built.
 * spec: draw/shared-builders#appearance
 */
export interface LegendMembers {
  /** Edge or corner of the plotting area the legend anchors to. Default `"top"`. */
  readonly legend_position?:
    | "top"
    | "bottom"
    | "left"
    | "right"
    | "top-left"
    | "top-right"
    | "bottom-left"
    | "bottom-right";
  /** Outside or inside the plotting area. Default `"outside"`. */
  readonly legend_placement?: "outside" | "inside";
  /** Legend title; unset draws none. */
  readonly legend_title?: string | null;
}
