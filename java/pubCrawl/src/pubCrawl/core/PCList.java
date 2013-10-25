package pubCrawl.core;

import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;

public class PCList extends PCObject implements Iterable {

    private final ArrayList<PCObject> _list = new ArrayList<PCObject>();

    public PCList() {

    }

    public PCList(List<PCObject> items) {
        for (PCObject item : items) {
            _list.add(item);
        }
    }

    public int size() {
        return _list.size();
    }

    public PCObject get(int idx) {
        return _list.get(idx);
    }

    public void set(int idx, PCObject val) {
        _list.set(idx, val);
    }

    public void add(PCObject val) {
        _list.add(val);
    }

    //  TODO update docs for removeAt
    public void removeAt(int idx) {
        _list.remove(idx);
    }

    public PCList subList(int start, int end) {
        return new PCList(_list.subList(start, end));
    }

    public Iterator<PCObject> iterator(){
        return _list.iterator();
    }

}
