// Description: This script is used to show a cookie popup on the website
// Get the cookie popup element
const cookiePopup = document.getElementById("cookie-popup");

// Get the accept button
const acceptCookies = document.getElementById("accept-cookies");

// Function to set a cookie
function setCookie(cname, cvalue, exdays) {
  const d = new Date();
  d.setTime(d.getTime() + exdays * 24 * 60 * 60 * 1000);
  const expires = "expires=" + d.toUTCString();
  document.cookie = cname + "=" + cvalue + ";" + expires + ";path=/";
}

// Function to check if the cookie has been set
function checkCookie() {
  const cookieAccepted = getCookie("cookieAccepted");
  if (cookieAccepted === "") {
    // Show the cookie popup
    cookiePopup.style.display = "block";
  }
}

// Function to get a cookie value
function getCookie(cname) {
  const name = cname + "=";
  const decodedCookie = decodeURIComponent(document.cookie);
  const ca = decodedCookie.split(";");
  for (let i = 0; i < ca.length; i++) {
    let c = ca[i];
    while (c.charAt(0) == " ") {
      c = c.substring(1);
    }
    if (c.indexOf(name) == 0) {
      return c.substring(name.length, c.length);
    }
  }
  return "";
}

// Event listener for the accept button
acceptCookies.addEventListener("click", function () {
  // Set the cookie
  setCookie("cookieAccepted", "true", 30);
  // Hide the cookie popup
  cookiePopup.style.display = "none";
});

// Call the checkCookie function
checkCookie();
