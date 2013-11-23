package pubCrawl.core;

import java.io.Serializable;
import java.util.HashMap;

public class PCObject implements Serializable {

    private final HashMap<String, Object> _props = new HashMap<String, Object>();
    private final static String _base = "050FCAC0-610C-4CF6-9CC2-5EA5A40C3155";

    public PCObject() {
        set(_base, this);
    }
    public PCObject(double num) {
        set(_base, num);
    }
    public PCObject(boolean b) {
        set(_base, b);
    }

    public PCObject set(String key, Object value) {
        _props.put(key, value);
        return this;
    }

    private Object getObj(String key) {
        return _props.containsKey(key) ? _props.get(key) : null;
    }

    @SuppressWarnings("unchecked")
    public <T> T get(String key) {
        // this is an unchecked cast and that's ok, it might fail at run time
        return (T) getObj(key);
    }

    public <T> T getBase() {
        return get(_base);
    }

}
