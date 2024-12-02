let getData = async day => {
  let fileName = `./src/Day${day->Int.toString}.txt`

  if RescriptBun.Fs.existsSync(fileName) {
    RescriptBun.Fs.readFileSync(fileName)->Buffer.toString
  } else {
    let data = await RescriptBun.Globals.Response.text(
      await RescriptBun.Globals.fetch(
        `https://adventofcode.com/2024/day/${day->Int.toString}/input`,
        ~init={
          headers: Globals.HeadersInit.FromArray([
            (
              "Cookie",
              `session=${RescriptBun.Bun.Env.get(RescriptBun.Bun.env, "SESSION")->Option.getOr(
                  "",
                )}`,
            ),
          ]),
        },
      ),
    )

    (
      await Bun.Write.write(
        ~destination=Bun.Write.Destination.fromPath(fileName),
        ~options={},
        ~input=Bun.Write.Input.fromString(data),
      )
    )->ignore

    data
  }
}
