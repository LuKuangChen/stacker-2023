// Adapted from https://www.sitepoint.com/get-url-parameters-with-javascript/

const queryString = window.location.search;
const urlBase = window.location.href.replace(window.location.search, '')
const urlParams = new URLSearchParams(queryString);
export const randomSeedAtURL = urlParams.get('randomSeed') || "";
export const nNextAtURL = parseInt(urlParams.get('nNext') || "0");
export const programAtURL = urlParams.get('program') || "";
export const make_url = (randomSeed, nNext, program) => {
    const params = new URLSearchParams();
    params.set('randomSeed', randomSeed);
    params.set('nNext', nNext);
    params.set('program', program);
    return `${urlBase}?${params.toString()}`;
};
export const shareLink = (randomSeed, nNext, program) => {
    alert(make_url(randomSeed, nNext, program))
};
