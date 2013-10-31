compute: pubCrawl/core/Compute.java pubCrawl/core/IPCFunction.java
	javac pubCrawl/core/Compute.java pubCrawl/core/IPCFunction.java
	jar cvf compute.jar pubCrawl/core/Compute.class pubCrawl/core/IPCFunction.class


server: compute
	javac -cp compute.jar pubCrawl/distributeServer/distributeServer.java pubCrawl/core/*.java


client: compute
	javac -cp compute.jar pubCrawl/distributeClient/*.java pubCrawl/core/*.java





.PHONY clean:
	find . -name *.class -exec rm '{}' ';'

