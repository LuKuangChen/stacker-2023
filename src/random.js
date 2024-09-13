export function make_random(seed) {
    // When no connected, Math.seedrandom might not exist.
    // This field of Math is loaded from an online package.
    if (Math.seedrandom) {
        return new Math.seedrandom(seed);
    } else {
        return Math.random;
    };
}
