// this is a comment

// ********
// TYPES
// ********

	// string
	name = 'Alden';
	hiThere = 'Hi, my name is {{name}}'; // built in formatting

	// number
	n1 = 4;  
	n2 = 5.0;

	// bool
	t = true;
	f = false;

	// object (JSON format)
	o1 = {}; 
	o2 = {
		p1: 'hey',
		p2: {
			p3: 7
		}
	};

	// list
	l1 = [];
	l2 = [1,2,3,4,5,6];
	one = l2[0];
	oneTwoThree = l2[0..2];
	copyL2 = l2[..];

// ********
// FUNCTIONS
// ********

	// declaration
	add = (x,y) => x+y;
	square = (x) => {
		x*x;
	};

	// calling
	seven = add(5,2);

	// partial params
	add5 = add(5);
	also7 = add5(2);

// ********
// IO
// ********

	// read
	input = read; // stdin
	input = read 'path/to/file.txt';

	// write
	print output; // stdout
	print output 'path/to/file.txt';

// ********
// PARALLEL
// ********

	// define task function 
	noSideEffects (input) => {
		// long process here
	};

	// distribute workload
	distribute


	

// *******
// WEB
// *******

	// Example 1 - fitbit needs to check on 4 sites

	pages <- ['amazon.com', 'bestbuy.com', 'radioshack.com', 'target.com'];


	// Example 2 - get 100 XKCD comics


	// Example 3 - build a reddit bot





