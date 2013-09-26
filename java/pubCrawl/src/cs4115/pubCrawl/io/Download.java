package cs4115.pubCrawl.io;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.util.EntityUtils;
import java.io.IOException;

public final class Download {

    public static String GetString(String url) throws IOException {
        HttpResponse response = GetResponse(url);
        HttpEntity entity = response.getEntity();
        return entity == null ? null : EntityUtils.toString(entity);
    }

    public static byte[] GetBytes(String url) throws IOException {
        HttpResponse response = GetResponse(url);
        HttpEntity entity = response.getEntity();
        return entity == null ? null : EntityUtils.toByteArray(entity);
    }

    private static HttpResponse GetResponse(String url) throws IOException{
        HttpClient client = new DefaultHttpClient();
        HttpGet get = new HttpGet(url);
        return client.execute(get);
    }

}
