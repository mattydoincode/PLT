public class List
{
	private static PCObject _obj = new PCObject();

	static {
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