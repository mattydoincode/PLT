public class Util
{
	public static IPCFunction map = new IPCFunction() {
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
	};

	public static IPCFunction where = new IPCFunction() {
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
	};
}