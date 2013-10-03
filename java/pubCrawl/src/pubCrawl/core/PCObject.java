package pubCrawl.core;

import java.util.HashMap;

public class PCObject {

    private HashMap<String, Object> _Map = new HashMap<String, Object>();
    private final static String _Base = "050FCAC0-610C-4CF6-9CC2-5EA5A40C3155";


    public PCObject(){

    }
    public PCObject(double num){
        Set(_Base,num);
    }
    public PCObject(boolean b){
        Set(_Base,b);
    }
    public void Set(String key, Object value){
        _Map.put(key,value);
    }

    public Object Get(String key) {
        if(_Map.containsKey(key)){
            return _Map.get(key);
        }
        return null;
    }

    public <T> T GetBase(){
        return (T) Get(_Base);
    }

    public <T> T GetCast(String key){
        return (T) Get(key);
    }





}
