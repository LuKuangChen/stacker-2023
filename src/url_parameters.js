// Adapted from https://www.sitepoint.com/get-url-parameters-with-javascript/

const queryString = window.location.search;
const urlBase = window.location.href.replace(window.location.search, '');
const urlParams = new URLSearchParams(queryString);
export const syntaxAtURL = urlParams.get('syntax') || "";
export const randomSeedAtURL = urlParams.get('randomSeed') || "";
export const nNextAtURL = parseInt(urlParams.get('nNext') || "-1");
export const programAtURL = urlParams.get('program') || "";
export const readOnlyMode = urlParams.get('readOnlyMode') !== null
export const make_url = (syntax, randomSeed, nNext, program, readOnlyMode) => {
    const params = new URLSearchParams();
    params.set('syntax', syntax);
    params.set('randomSeed', randomSeed);
    params.set('nNext', nNext);
    params.set('program', program);
    if (readOnlyMode) {
        params.set('readOnlyMode', "")
    }
    return `${urlBase}?${params.toString()}`;
};
