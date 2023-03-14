// Adapted from https://www.sitepoint.com/get-url-parameters-with-javascript/

const queryString = window.location.search;
const urlParams = new URLSearchParams(queryString);
export const randomSeedAtURL = urlParams.get('randomSeed') || ""
export const programAtURL = urlParams.get('program') || ""
