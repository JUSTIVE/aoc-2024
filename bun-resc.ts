import { plugin, file } from "bun";
import { execSync } from "child_process";

await plugin({
  name: "rescript loader",
  async setup({ onLoad }) {
    execSync("bun res:build");
    return onLoad(
      { filter: /\.res$/ },
      async ({ path }) => ({ contents: await file(`${path}.mjs`).text() })
    );
  },
});