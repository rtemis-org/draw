/**
 * The ECharts module and instance the fit steps take from their host. The
 * steps read ECharts' component models and views (`getModel()`,
 * `getViewOfComponentModel()`), for which ECharts publishes no types, and the
 * package takes ECharts from the host rather than depending on it, so both
 * are typed loosely.
 */

// biome-ignore lint/suspicious/noExplicitAny: ECharts' model and view API has no published types.
export type EChartsModule = any;

// biome-ignore lint/suspicious/noExplicitAny: ECharts' model and view API has no published types.
export type EChartsInstance = any;

/** An option or component option, as the host set it. */
export type OptionRecord = Record<string, unknown>;

/**
 * An option read by a fit step, whose members the step reads without a
 * declared shape, as ECharts reads them.
 */
// biome-ignore lint/suspicious/noExplicitAny: an option's members have the shapes ECharts accepts.
export type LooseOption = Record<string, any>;
