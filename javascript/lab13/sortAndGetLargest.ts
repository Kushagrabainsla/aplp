function swap(arr: number[], i: number, j: number): void {
    const temp: number = arr[i];
    arr[i] = arr[j];
    arr[j] = temp;
}

function sortAndGetLargest(arr: number[]): number {
    let max: number = arr[0];
    let i: number;
    let j: number;

    for (i = 0; i < arr.length; i += 1) {
        if (arr[i] > max) {
            max = arr[i];
        }
        for (j = i + 1; j < arr.length; j += 1) {
            if (arr[i] < arr[j]) {
                swap(arr, i, j);
            }
        }
    }
    return max;
}

const largest: number = sortAndGetLargest([99, 2, 43, 8, 0, 21, 12]);
console.log(largest);
