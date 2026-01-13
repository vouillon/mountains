//Provides: inflate_into
async function inflate_into(compressed, target, offset = 0) {
    const ds = new globalThis.DecompressionStream('deflate');
    const stream = new globalThis.ReadableStream({
        start(controller) {
            controller.enqueue(compressed);
            controller.close();
        },
    }).pipeThrough(ds);

    const reader = stream.getReader();
    let current_offset = offset;
    while (true) {
        const { done, value } = await reader.read();
        if (done) break;
        target.set(value, current_offset);
        current_offset += value.length;
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
