//#############USE CASE 1, NON DISTRIBUTED, JUST SHOWING STUFF #############


//bubble sort
bubble(values) -> {
	swapped = true;
	while(swapped) {
		swapped = false;
		for(var i = 0; i < values.length-1; i++) {
			if (values[i] > values[i+1]) {
				var temp = values[i];
				values[i] = values[i+1];
				values[i+1] = temp;
				swapped = true;
		  	}
		}
	}
	//return statement??
}

mylist = [7, 4, 5, 2, 9, 1];
mylist.bubble();
print(mylist); // ??



//#############USE CASE 2, DISTRIBUTED NETWORKING ################
//open??

getComic (id) -> {
	wholeSite = download("http://xkcd.com/" + id);
	imgChunk = wholeSite.split('<').where(x->x.contains("imgs.xkcd.com/comics"));
	if(imgChunk.length!=1){
		//throw error!
		//this should count as a return essentially
	}
	else{

		httpStart = imgChunk.getIndexOf("http");
		jpgEnd = imgChunk.getIndexOf("jpg") + 3;
		return download(imgChunk.substring(httpStart, jpgEnd)); //should we handle substring differently?
		//alternate imgChunk[httpStart:jpgEnd] ??
	}
}

range (count) -> {
	mylist = [];
	for(var i = 0; i < count; i ++){
		list.add(i);
	}
	return list;
}

pics = range(100).distribute(x - > getComic(x)).collect();
count = 0;
for(result in pics){
	pic = results.output;
	print(pic, "xkcdimg" + count++ + ".jpg"); //would this work? for a couple of questions
}

//close??

//########## USE CASE 3, DISTRIBUTED MATH? ###########


getPrimes (max) -> {

	myPrimes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,
				89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,
				191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,
				307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,
				431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,
				557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,
				661,673,677,683,691,701,709,719,727,733,739,743,751,757,761,769,773,787,797,
				809,811,821,823,827,829,839,853,857,859,863,877,881,883,887,907,911,919,929,
				937,941,947,953,967,971,977,983,991,997];
	return myPrimes.where(x->x < max);
}

trial_division(n) ->{
    """Return a list of the prime factors for a natural number."""
    if(n == 1){
    	return [1];
    }
    primes = getPrimes(sqrt(n) + 1); //sqrt??
    prime_factors = [];

    for(p in primes) {
        while n % p == 0{
            prime_factors.add(p);
            n = n/p;
        }
    }
    if (n > 1){ 
    	prime_factors.add(n)
    }
    return prime_factors
}

// prime factorize every number up to 1000

print(range(1000).distribute(trial_division).collect().select(x=>x.output + "\n"));//can we shortcut function calling?