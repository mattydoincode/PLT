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
		},
		myFunc: (x,y) => x + y
	};
	
	// object access
	thisIsHey = o2.p2;
	alsoHey = o2['p2'];

	// list
	l1 = [];
	l2 = [1,2,3,4,5,6];
	one = l2[0];
	oneTwoThree = l2[0:2];
	copyL2 = l2[:];

// ********
// FUNCTIONS
// ********

	// declaration
	add(x,y) -> x+y;
	square(x) -> {
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
	
	// open slave
	open('192.168.4.03');
	open(['192.168.4.01', '192.168.4.02', '192.168.4.03']);

// ********
// PARALLEL
// ********

	results = [1, 2, 3, 4].distribute(x -> square(x));
	
	filtered = results.where(x -> {
		x % 2 == 0;
	});

	// define task function 
	noSideEffects (input) => {
		// long process here
	};

	// distribute workload
	distribute


// ********
// COMMENTS
// ********


