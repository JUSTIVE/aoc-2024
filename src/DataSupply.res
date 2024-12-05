let getData = async (year, day, session) => {
  let fileName = `./src/${year->Int.toString}/Day${day->Int.toString}.txt`

  if RescriptBun.Fs.existsSync(fileName) {
    RescriptBun.Fs.readFileSync(fileName)->Buffer.toString
  } else {
    let data = await RescriptBun.Globals.Response.text(
      await RescriptBun.Globals.fetch(
        `https://adventofcode.com/${year->Int.toString}/day/${day->Int.toString}/input`,
        ~init={
          headers: Globals.HeadersInit.FromArray([("Cookie", `session=${session}`)]),
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
