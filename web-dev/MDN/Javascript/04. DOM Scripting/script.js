
const list = document.querySelector("ul");
const input = document.querySelector("input");
const button = document.querySelector("button");

button.addEventListener("click", e => addListItem(e));



function addListItem(e) {
    const newItem = input.value;
    input.value = '';

    const li = document.createElement("li");
    const span = document.createElement("span");
    const button = document.createElement("button");

    li.appendChild(span);
    li.appendChild(button);

    span.textContent = newItem;
    button.textContent = "Delete";

    list.appendChild(li);

    button.addEventListener("click", e => li.remove());

    input.focus();
}