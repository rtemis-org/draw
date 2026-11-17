export type {
  ConfusionAppearance,
  ConfusionChartConfig,
  ConfusionData,
  ConfusionLayout,
  ConfusionPanel,
  ConfusionRates,
} from "./config.ts";
export {
  buildConfusionOption,
  CONFUSION_CHART_CONFIG_MEMBERS,
  confusionLayout,
  confusionRates,
} from "./config.ts";
export type { ConfusionPayload } from "./fit.ts";
export { confusionHeight, fitConfusion } from "./fit.ts";
