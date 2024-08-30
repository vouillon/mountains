//Provides: inflate
function toReadableStream(value) {
	return new globalThis.ReadableStream({
		start(controller) {
			controller.enqueue(value);
			controller.close();
		},
	});
}
function concatArrayBuffers(chunks) {
    const result = new Uint8Array(chunks.reduce((a, c) => a + c.length, 0));
    let offset = 0;
    for (const chunk of chunks) {
        result.set(chunk, offset);
        offset += chunk.length;
    }
    return result;
}
async function streamToArrayBuffer(stream) {
    const chunks = [];
    const reader = stream.getReader();
    while (true) {
        const { done, value } = await reader.read();
        if (done) {
            break;
        } else {
            chunks.push(value);
        }
    }
    return concatArrayBuffers(chunks);
}

function inflate (s) {
  var s = toReadableStream(s).pipeThrough(new globalThis.DecompressionStream('deflate'))
  return streamToArrayBuffer(s);
}
