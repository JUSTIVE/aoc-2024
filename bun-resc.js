import { plugin, file } from "bun";
import { exec } from "child_process";

await plugin({
  name: "rescript loader",
  async setup({ onLoad }) {
    await exec("bun res:build");
    return onLoad(
      { filter: /\.res$/ },
      async ({ path }) => ({ contents: await file(`${path}.mjs`, "utf8").text() })
    );
  },
});