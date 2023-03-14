// Adapted from https://www.sitepoint.com/get-url-parameters-with-javascript/

const queryString = window.location.search;
const urlParams = new URLSearchParams(queryString);
export const randomSeedAtURL = urlParams.get('randomSeed') || "";
export const programAtURL = urlParams.get('program') || "";
export const make_url = (randomSeed, program) => {
    const base = window.location.href.replace(window.location.search, '')
    const params = new URLSearchParams();
    params.set('randomSeed', randomSeed);
    params.set('program', program);
    return `${base}?${params.toString()}`;
};
export const shareLink = (randomSeed, program) => {
    alert(make_url(randomSeed, program))
};
