
// define function "doMath" that takes 2 params
doMath(x, y) -> {
	return x * y;
}

// define object with property "doMath" that takes 1 param
myObj = {
	'doMath': (x) -> x + 4
};

// to call outer function is simple:
eight = doMath(2, 4);

// to call obj function is also simple:
seven = myObj.doMath(3);

// BUT WAIT, this is ambiguous!
// because we allow this:
nine = 3.doMath(3); // equivalent to "nine = doMath(3, 3);"

// how do we know we didn't want:
seven = doMath(myObj, 3);
