const displayedImage = document.querySelector('.displayed-img');
const thumbBar = document.querySelector('.thumb-bar');

const btn = document.querySelector('button');
const overlay = document.querySelector('.overlay');

const images = ["pic1.jpg", "pic2.jpg", "pic3.jpg", "pic4.jpg", "pic5.jpg"];

const altTexts = ["Closeup of a human eye", "Closeup of a limestone rock formation", 
    "Closeup of violets", "Photo of Egyptian hieroglyphs", "A large butterfly on a leaf"];

/* Looping through images */
for (let i=0; i<5; i++) {
    const newImage = document.createElement('img');
    newImage.setAttribute('src', `images/${images[i]}`);
    newImage.setAttribute('alt', altTexts[i]);
    thumbBar.appendChild(newImage);

    // newImage.addEventListener("click", e => displayImage(e));
}

thumbBar.addEventListener("click",  e => displayImageContainer(e));

btn.addEventListener("click", e => buttonLight(e));




function displayImage(e) {
    displayedImage.setAttribute("src", e.target.src);
    displayedImage.setAttribute("alt", e.target.alt);
}

function displayImageContainer(e) {
    e.stopPropagation();
    displayedImage.setAttribute("src", e.target.src);
    displayedImage.setAttribute("alt", e.target.alt);
}

function buttonLight(e) {
    if (btn.getAttribute("class") === "dark") {
        btn.setAttribute("class", "light");
        btn.textContent = "Lighten";
        overlay.style.backgroundColor = `rgb(0 0 0 / 50%)`;
    } else {
        btn.setAttribute("class", "dark");
        btn.textContent = "Darken";
        overlay.style.backgroundColor = `rgb(0 0 0 / 0%)`;
    }
}