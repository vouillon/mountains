//Provides: inflate_into
async function inflate_into(compressed, target) {
    const ds = new globalThis.DecompressionStream('deflate');
    const stream = new globalThis.ReadableStream({
        start(controller) {
            controller.enqueue(compressed);
            controller.close();
        },
    }).pipeThrough(ds);

    const reader = stream.getReader();
    let offset = 0;
    while (true) {
        const { done, value } = await reader.read();
        if (done) break;
        target.set(value, offset);
        offset += value.length;
    }
}

//Provides: inflate
async function inflate(compressed) {
    const ds = new globalThis.DecompressionStream('deflate');
    const stream = new globalThis.ReadableStream({
        start(controller) {
            controller.enqueue(compressed);
            controller.close();
        },
    }).pipeThrough(ds);
    const buf = await new globalThis.Response(stream).arrayBuffer();
    return new Uint8Array(buf);
}
