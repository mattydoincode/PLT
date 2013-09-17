PLT
===

Hey guys, 

So I was thinking about the "GET and then later on Collect" problem and (I'm not sure if this would be able to solve it, but might) decided to crack open my Haskell book. We could possibly take the functional approach to it and use "referential transparency" when we call the GET function/thing. Using something like this, we could make sure our language is "lazy," meaning that, unless specifically told otherwise, it won't execute the collect function (or any other thing we think of) until it needs to show you a result. If you know the result of the function (say you wrote a function to scrape specific eBay pages) dependsonly on the parameters that function is given, it doesn't matter when you actually calculate the result of the function. So with this, the language defers actually computing the result as long as possible.

This laziness would allow us to ceeate seemingly infinite data structures (like the fact the depth of the crawler could go on, and on, and on...) because only the parts of the datastructure (or website) that we choose to display will actually be computed.

Say you want to crawl reddit and you make a function to find the next button and keep pressing it (or getting the link to the next page). Now instead of imperatively looping until you get the depth you want, now we can just do getNextReddit(getNextReddit(getNextReddit(www.reddit.com))). Calling that function on that "list" of pages it is crawling would allow a "lazy" language to say "yeah, yeah, I'll do it later". Once you want to see the result, the first getNextReddit calls the second one and says it wants the result immediately,. Then the second one says the same thing and so on until it gets to the last one, and then passes the result back to the first one.


After typing all that out, I actually have no idea if this would work or apply to our problem - so if it sounds completely retarded, let me know.
