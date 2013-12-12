public class List
{
	private static PCObject _obj = new PCObject();

	static {

		// TODO SIREESH
		// see the LRM (in the final report) for details on any of these
		/*
			List add(list, item)
			List remove(list, idx)
			int find(list, sublist)
			List<List> split(list, item)
			List range(min, max)
			int length(list)
		*/

		_obj.set("map", new IPCFunction() {
			@Override
			public PCObject call(PCObject... args) {
				PCList list = (PCList)args[0];
				IPCFunction mapper = (IPCFunction)args[1];
				PCList newList = new PCList();
				for (PCObject o : list) {
					newList.add(mapper.call(o));
				}
				return newList;
			}
		});

		_obj.set("where", new IPCFunction() {
			@Override
			public PCObject call(PCObject... args) {
				PCList list = (PCList)args[0];
				IPCFunction filter = (IPCFunction)args[1];
				PCList newList = new PCList();
				for (PCObject o : list) {
					if (filter.call(o).<Boolean>getBase()) {
						newList.add(o);
					}
				}
				return newList;
			}
		});
	}

    public static <T> T get(String key) {
    	return _obj.<T>get(key);
    }
}