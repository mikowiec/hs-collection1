
function signCts(code,key,target) {

    target.value = hex_hmac_sha1(key.value, code.value);
}

