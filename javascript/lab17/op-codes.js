'use strict';

let opcodes = {
  0x01: {
    mnemonic: 'ADD', evaluate: (vm) => {
      let v1 = vm.stack.pop();
      let v2 = vm.stack.pop();
      vm.stack.push(v1 + v2);
    }
  },
  0x02: {
    mnemonic: 'MUL', evaluate: (vm) => {
      let v1 = vm.stack.pop();
      let v2 = vm.stack.pop();
      vm.stack.push(v1 * v2);
    }
  },
  0x03: {
    mnemonic: 'SUB', evaluate: (vm) => {
      let v1 = vm.stack.pop();
      let v2 = vm.stack.pop();
      vm.stack.push(v1 - v2);
    }
  },
  0x90: {
    mnemonic: 'SWAP1', evaluate: (vm) => {
      let v1 = vm.stack.pop();
      let v2 = vm.stack.pop();
      vm.stack.push(v1);
      vm.stack.push(v2);
    }
  },
  0x52: {
    mnemonic: 'MSTORE', evaluate: (vm) => {
      let offset = vm.stack.pop();
      let value = vm.stack.pop();
      vm.memory[offset] = value;
    }
  },
  0x51: {
    mnemonic: 'MLOAD', evaluate: (vm) => {
      let offset = vm.stack.pop();
      vm.stack.push(vm.memory[offset]);
    }
  },
  0x56: {
    mnemonic: 'JUMP', evaluate: (vm) => {
      let dest = vm.stack.pop();
      vm.pc = dest - 1; // Subtracted 1 as vm.pc++ happens at the end of the loop
    }
  },
  0x57: {
    mnemonic: 'JUMPI', evaluate: (vm) => {
      let dest = vm.stack.pop();
      let condition = vm.stack.pop();
      if (condition) {
        vm.pc = dest - 1;
      }
    }
  },
  0x5B: {
    mnemonic: 'JUMPDEST', evaluate: (vm) => {
      // Does nothing.  We could check to make sure that jumps
      // always land at JUMPDEST opcodes, but it is not totally
      // clear that it is worth the bother.
    }
  },
  0x60: {
    mnemonic: 'PUSH1', evaluate: (vm) => {
      // The next byte is data, not another instruction
      vm.pc++;
      let v = vm.bytecode.readUInt8(vm.pc);
      vm.stack.push(v);
    }
  },
  0x0c: {
    mnemonic: 'PRINT', evaluate: (vm) => {
      // **NOTE**: This is not a real EVM opcode.
      console.log(vm.stack.pop());
    }
  },
};

exports.opcodes = opcodes;
