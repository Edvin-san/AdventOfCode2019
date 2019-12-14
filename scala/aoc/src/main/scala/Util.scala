object Util {
	def lowerBound[V <% Ordered[V]](first: BigInt, last: BigInt, value: V, valueOf: BigInt => V): BigInt = {
		// Adjusted from https://en.cppreference.com/w/cpp/algorithm/lower_bound
		var f = first
		var count: BigInt = last - first

		while (count > 0) {
			val step = count / 2
			val i = f + step
			if (valueOf(i) < value) {
				f = i + 1
				count -= step + 1
			} else count = step
		}

		return f
	}
}