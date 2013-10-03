package pubCrawl.core;

import java.util.ArrayList;
import java.util.List;

public class PCList {
    private ArrayList<PCObject> _List = new ArrayList<PCObject>();

    public PCList(){

    }

    public PCList(List<PCObject> l){
        for(int i = 0; i < l.size(); i++){
            _List.add(l.get(i));
        }
    }

    public int Length(){
        return _List.size();
    }

    public PCObject Get(int idx){
        return _List.get(idx);
    }

    public void Set(int idx, PCObject val){
        _List.set(idx,val);
    }

    public void Add(PCObject val){
        _List.add(val);
    }

    //  TODO update docs for removeAt
    public void RemoveAt(int idx){
        _List.remove(idx);
    }

    public PCList SubList(int start, int end){
        return new PCList(_List.subList(start,end));
    }


}
