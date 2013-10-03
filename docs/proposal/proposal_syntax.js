// ********
// COMMENTS
// ********

	// single line comments start 

	/*
		multi-line comments
	*/

	/*
		/* totally breaks */
		// totally work
	*/


	/* this will break too /* */

// ********
// TYPES
// ********

	// string, single quotes
	name = 'Alden';
	hiThere = 'Hi, my name is {{name}}'; // built in formatting
	hereisaquote = '\''; // backslash escape

	// number
	n1 = 4;  
	n2 = 5.0;

	// bool
	b1 = true;
	b2 = false;

	// object (JSON format)
	o1 = {}; 
	o2 = {
		p1: 'hey',
		'p2': {
			p3: 7
		},
		myFunc: (x,y) -> x + y
	};
	
	// object access (object is really just a HashTable)
	thisIsHey = o2.p2;
	alsoHey = o2['p2']; // must be a string, o2[7] is invalid

	// so, if a number or anything with '.' is put in, MUST access with []
	weirdObj = {
		'72': 'hey',
		'c.o.o.l': 'dude'
	};
	weirdObj.72; // fails
	weirdObj['72']; // 'hey'
	weirdObj.c.o.o.l; // fails
	weirdObj['c.o.o.l.']; // 'dude'

	// keys is also available to all objects
	k = o2.keys; // ['p1', 'p2', 'myFunc'];

	// list creation
	l1 = [];
	l2 = [1,2,3,4,5,6];

	// list access
	five = l2[4];
	oneTwoThree = l2[0:2];
	copyOfL2 = l2[:];

	// list modification
	oneTwoThree.add(4);
	oneTwoThree.remove(2);
	// oneTwoThree is now [1,3,4]

// ********
// CONTROL FLOW
// ********

	// conditionals (else is not required)
	if (bool) {
		// do stuff here
	}
	elif (otherBool) {
		// more stuff here
	}
	else {
		// other stuff
	}

	// foreach loops
	for (item in list) {
		
	}

	// for loops
	for (i = 0; i < 10; i++) {

	}

	// while loops
	while (bool) {

	}

// ********
// FUNCTIONS
// ********

	// declaration
	add(x,y) -> x+y;
	square(num) -> {
		if (true) {
			return num*num;
		}
		return 7; // return statement REQUIRED if braces
	} // no semi-colon needed to end function if braces

	// calling
	seven = add(5,2);

	// all functions can also be called as extensions on the first argument
	alsoSeven = 5.add(2);

// ********
// IO
// ********

	

	// read
	input = read(); // stdin
	input = read('path/to/file.txt'); //returns list of strings, each being a line

	output = "Hello World";

	// write
	print(output); // stdout
	print(output, 'path/to/file.txt');

// ********
// PARALLEL
// ********


	// one magic function, distribute

	urls = ['google.com', 'apple.com', 'amazon.com'];

	results = urls.distribute(x -> getPageTitle(x));
	// getPageTitle is definted somewhere else as getPageTitle(url) -> // download page, etc.

	// results is a list of special objects
	firstResult = results[0];
	/*
		{
			'ip': '129.324.1.1', // IP address of slave (or self) where task ran
			'error': '', // error message if something failed
			'input': 'amazon.com', // input object for task (in this case just string)
			'output': 'Amazon.com: Online Shopping for...' // result of task (in this case just string)
		}
	*/

// ********
// LIST UTILITIES
// ********

	// part of a standard library to make list processing easier

	// filtering
	filtered = results.where(x -> {
		return x % 2 == 0;
	});

	alsoFiltered = results.where(x -> x % 2 == 0);

	// mapping
	mapped = results.map(x -> x.name);
	alsoMapped = results.map(x -> {
		if (x == 5) {
			return 9;
		} else {
			return 8;
		}
	});

	// implementation
	where(list, predicate) -> {
		if (!list) {
			return null;
		}
		newList = [];
		for (item in list) {
			if (predicate(item)) {
				newList.add(item);
			}
		}
		return newList;
	}
	map(list, selector) {
		if (!list) {
			return null;
		}
		newList = [];
		for (item in list) {
			newList.add(selector(item));
		}
		return newList;
	}


	list utilities :

	find = indexof
	contains?
	any?
	split?


//##########what matt thought of!#################

//array stuff
array.length

//string stuff?
substrings handled like arrays

"http://xkcd.com/" + id //does this work?


//etc..

//operators.. what's defined?
i++ //only fucking thing defined
//maybe more math??
% //yeah iguess we have mod
//etc..

//always pass by value bitches

//negative values
null// is falsy cuz it's nice ??? maybe if everything goes well
0// is zero and it's not falsey, numbers don't work as bools
false, true// are only bool things


//what if we print a list or an object? how does it handle that?
print([1,2,3]); //1, 2, 3 ??

//boom

//exceptions
