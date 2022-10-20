const fs = require('fs')
const child_process = require('child_process')
const assert = require('assert')

function load(exports, addr, cell) {
    return exports.memory[addr / 4 + cell]
}

function store(exports, addr, cell, value) {
    exports.memory[addr / 4 + cell] = value
}

function alloc(heap, words) {
    const addr = heap.value
    heap.value = addr + words * 4
    return addr
}

function car(exports, list) {
    return load(exports, list, 0)
}

function cdr(exports, list) {
    return load(exports, list, 1)
}

function create_pair(exports, fst, snd) {
    const addr = alloc(exports.heap, 2)
    store(exports, addr, 0, fst)
    store(exports, addr, 1, snd)
    return addr
}

function push(exports, value) {
    exports.stack.value = create_pair(exports, value, exports.stack.value)
}

function stack_top(exports) {
    return load(exports, exports.stack.value, 0)
}

function encodeValue(exports, value) {
    if ('int' in value) {
        return value.int
    }

    if ('string' in value) {
        let addr = alloc(exports.heap, Math.ceil(value.string.length / 4))
        for (let i = 0; i < value.string.length; i++) {
            exports.buffer[addr + i] = value.string[i]
        }

        return addr
    }

    if ('prim' in value) {
        switch (value.prim) {
            case 'Left': {
                let wrapped = encodeValue(exports, value.args[0])
                return create_pair(exports, 1, wrapped)
            }
            case 'Right': {
                let wrapped = encodeValue(exports, value.args[0])
                return create_pair(exports, 0, wrapped)
            }
            case 'Unit': {
                return 0
            }
        }
    }
}

function inspect_all(exports) {
    console.log('Stack pointer ', exports.stack.value)
    console.log('Heap pointer ', exports.heap.value)
    console.log('Stack')

    let stack = exports.stack.value
    while (true) {
        if (stack === 0) {
            console.log(' -> nil')
            break
        }

        const value = car(exports, stack)
        stack = cdr(exports, stack)
        console.log(' ->', value)
    }

    console.log('Heap')
    for (let i = 512; i <= exports.heap.value; i += 4) {
        console.log(' ', load(exports, i, 0))
    }
}

function compileMichelsonCode(code) {
    const p = child_process.exec('/home/renato/workspace/tunac/_build/default/main.exe')

    p.stdin.end(code)
    p.stderr.pipe(process.stderr)

    return new Promise((resolve, _) => {
        let buf = ''
        p.stdout.on('data', chunk => buf += chunk)
        p.stdout.on('end', () => {
            resolve(Buffer.from(buf)) 
        })
    })
}

async function wasmModuleOfMichelson(code) {
    await compileMichelsonCode(code)
    const wasm = fs.readFileSync('./mod.wasm')
    return WebAssembly.compile(wasm)
}

async function eval(code, parameter, storage) {
    const module = await wasmModuleOfMichelson(code)
    const instance = new WebAssembly.Instance(module)

    const memory = instance.exports.memory.buffer
    const words = new Uint32Array(memory)

    const exports = {
        memory: words,
        buffer: memory,
        heap: instance.exports.heap_top,
        stack: instance.exports.stack
    }

    parameter = encodeValue(exports, parameter)
    storage = encodeValue(exports, storage)
    push(exports, create_pair(exports, parameter, storage))

    // inspect_all(exports)
    instance.exports.main()
    // inspect_all(exports)

    return exports
}

async function main() {
    let exports = await eval(`
        { parameter unit; storage int; code { CDR; NIL operation; PAIR } }
    `, { prim: 'Unit', args: [], annots: [] }, { int: 42 })
    // inspect_all(exports)
    assert(cdr(exports, stack_top(exports)) === 42)

    exports = await eval(`
        { parameter int; storage int; code { UNPAIR; ADD; NIL operation; PAIR } }
    `, { int: 13 }, { int: 42 })
    assert(cdr(exports, stack_top(exports)) === 55)

    exports = await eval(`
        { parameter (or (or int int) unit);
          storage int;
          code { UNPAIR; IF_LEFT { IF_LEFT { SWAP; SUB } { ADD } } { PUSH int 0 }; NIL operation; PAIR } }
    `, { prim: 'Left', args: [ { prim: 'Right', args: [ { int: 13 } ], annots: [] } ], annots: [] }, { int: 42 })
    assert(cdr(exports, stack_top(exports)) === 55)

    exports = await eval(`
        { parameter (or (or int int) unit);
          storage int;
          code { UNPAIR; IF_LEFT { IF_LEFT { SWAP; SUB } { ADD } } { PUSH int 0 }; NIL operation; PAIR } }
    `, { prim: 'Left', args: [ { prim: 'Left', args: [ { int: 13 } ], annots: [] } ], annots: [] }, { int: 42 })
    assert(cdr(exports, stack_top(exports)) === 29)
    // inspect_all(exports)

    exports = await eval(`
        { parameter (or (or int int) unit);
          storage int;
          code { UNPAIR; IF_LEFT { IF_LEFT { SWAP; SUB } { ADD } } { PUSH int 0 }; NIL operation; PAIR } }
    `, { prim: 'Right', args: [ { prim: 'Unit', args: [], annots: [] } ], annots: [] }, { int: 42 })
    assert(cdr(exports, stack_top(exports)) === 0)
}

main()