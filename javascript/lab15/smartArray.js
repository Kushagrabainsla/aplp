// Lab 15: SmartArray using Proxy
// Features:
// 1) Range reads: smartArray['2-4']
// 2) Negative index reads/writes: smartArray[-1]
// 3) Non-integer numeric indexes throw an error

function isRangeKey(propertyKey) {
  return typeof propertyKey === 'string' && /^-?\d+-\d+$/.test(propertyKey);
}

function parseRange(propertyKey) {
  const [startIndexText, endIndexText] = propertyKey.split('-');
  const rangeStartIndex = Number(startIndexText);
  const rangeEndIndex = Number(endIndexText);
  return { rangeStartIndex, rangeEndIndex };
}

function isIntegerIndexKey(propertyKey) {
  return typeof propertyKey === 'string' && /^-?\d+$/.test(propertyKey);
}

function isNumericButNotIntegerKey(propertyKey) {
  if (typeof propertyKey !== 'string' || propertyKey.trim() === '') {
    return false;
  }

  const numericPropertyValue = Number(propertyKey);
  return Number.isFinite(numericPropertyValue) && !Number.isInteger(numericPropertyValue);
}

function normalizeIndex(index, length) {
  return index < 0 ? length + index : index;
}

function createSmartArray(initialValues = []) {
  const underlyingArray = Array.from(initialValues);

  return new Proxy(underlyingArray, {
    get(arrayTarget, propertyKey, proxyReceiver) {
      if (typeof propertyKey === 'symbol') {
        return Reflect.get(arrayTarget, propertyKey, proxyReceiver);
      }

      // Range lookup, inclusive of both ends.
      if (isRangeKey(propertyKey)) {
        const { rangeStartIndex, rangeEndIndex } = parseRange(propertyKey);

        if (rangeStartIndex <= rangeEndIndex) {
          return arrayTarget.slice(rangeStartIndex, rangeEndIndex + 1);
        }

        return [];
      }

      if (isNumericButNotIntegerKey(propertyKey)) {
        throw new TypeError(`Invalid index: ${propertyKey}. Index must be an integer.`);
      }

      if (isIntegerIndexKey(propertyKey)) {
        const requestedIndex = Number(propertyKey);
        const resolvedIndex = normalizeIndex(requestedIndex, arrayTarget.length);

        if (resolvedIndex < 0) {
          return undefined;
        }

        return arrayTarget[resolvedIndex];
      }

      return Reflect.get(arrayTarget, propertyKey, proxyReceiver);
    },

    set(arrayTarget, propertyKey, assignedValue, proxyReceiver) {
      if (typeof propertyKey === 'symbol') {
        return Reflect.set(arrayTarget, propertyKey, assignedValue, proxyReceiver);
      }

      if (isNumericButNotIntegerKey(propertyKey)) {
        throw new TypeError(`Invalid index: ${propertyKey}. Index must be an integer.`);
      }

      if (isIntegerIndexKey(propertyKey)) {
        const requestedIndex = Number(propertyKey);
        const resolvedIndex = normalizeIndex(requestedIndex, arrayTarget.length);

        if (resolvedIndex < 0) {
          throw new RangeError(`Index ${requestedIndex} is out of bounds.`);
        }

        arrayTarget[resolvedIndex] = assignedValue;
        return true;
      }

      return Reflect.set(arrayTarget, propertyKey, assignedValue, proxyReceiver);
    },

    deleteProperty(arrayTarget, propertyKey) {
      if (typeof propertyKey === 'symbol') {
        return Reflect.deleteProperty(arrayTarget, propertyKey);
      }

      if (isNumericButNotIntegerKey(propertyKey)) {
        throw new TypeError(`Invalid index: ${propertyKey}. Index must be an integer.`);
      }

      if (isIntegerIndexKey(propertyKey)) {
        const requestedIndex = Number(propertyKey);
        const resolvedIndex = normalizeIndex(requestedIndex, arrayTarget.length);

        if (resolvedIndex < 0) {
          return true;
        }

        return Reflect.deleteProperty(arrayTarget, String(resolvedIndex));
      }

      return Reflect.deleteProperty(arrayTarget, propertyKey);
    }
  });
}

const smartArray = createSmartArray([10, 20, 30, 40, 50]);

console.log('range 2-4:', smartArray['2-4']);     // [30, 40, 50]
console.log('last item:', smartArray[-1]);         // 50
smartArray[-1] = 99;
console.log('new last item:', smartArray[-1]);     // 99

delete smartArray[-2];
console.log('after delete -2:', smartArray);       // [10, 20, 30, <empty>, 99]

try {
  console.log(smartArray['2.5']);
} catch (err) {
  console.log('error:', err.message);
}

module.exports = { createSmartArray };
