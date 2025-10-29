// Examine the document object
// console.log(document.URL);
// console.log(document.title);
// // document.title = 123;
// console.log(document.doctype);
// console.log(document.head);
// console.log(document.body);
// console.log(document.forms);
// console.log(document.links);
// console.log(document.images);

// Get elements
var header = document.getElementById('main-header');
console.log(header);

var listItems = document.getElementsByClassName('list-group-item');
// console.log(listItems[1].textContent);

var items2 = document.getElementsByTagName('li');
// console.log(items2);


var mainHeader = document.querySelector("#main-header");
header.style.borderBottom = 'solid 2px #000';

var input = document.querySelector('input');
input.value = 'hello';

var submit = document.querySelector('input[type="submit"');
submit.value='Send';

var listItem = document.querySelector('.list-group-item:last-child');
listItem.style.color = 'blue';