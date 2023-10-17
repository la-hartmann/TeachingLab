/* Use JavaScript files under www/ anytime you are writing substantial logic that’s specific to a single app */
function decodeJwtResponse(token) {
    let base64Url = token.split('.')[1]
    let base64 = base64Url.replace(/-/g, '+').replace(/_/g, '/');
    let jsonPayload = decodeURIComponent(atob(base64).split('').map(function(c) {
        return '%' + ('00' + c.charCodeAt(0).toString(16)).slice(-2);
    }).join(''));
    return JSON.parse(jsonPayload)
}

let responsePayload;
window.handleCredentialResponse = async (response) => {
  // decodeJwtResponse() is a custom function defined by you
  // to decode the credential response.
  responsePayload = decodeJwtResponse(response.credential);

  Shiny.onInputChange("g.id", responsePayload.sub)
  Shiny.onInputChange("g.name", responsePayload.name)
  Shiny.onInputChange("g.image", responsePayload.picture)
  Shiny.onInputChange("g.email", responsePayload.email)
}
