// Per-icon ESM paths from @ant-design/icons don't ship .d.ts entries.
// Declaring them here keeps tree-shaking effective without forcing the
// build to import the full icons index.
declare module "@ant-design/icons/es/icons/*" {
  import { ForwardRefExoticComponent, RefAttributes } from "react";
  // Minimal surface — matches the AntdIconProps used at the call sites.
  const Icon: ForwardRefExoticComponent<
    Record<string, unknown> & RefAttributes<HTMLSpanElement>
  >;
  export default Icon;
}
