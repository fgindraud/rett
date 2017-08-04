/* Rett - Relation Editor and Tracking Tool
 * Copyright (C) 2017 Francois Gindraud
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <QApplication>
#include <QTimer>

#include <cassert>
#include <duck/variant.h>
#include <unordered_map>
#include <vector>

#include <duck/range/range.h>
#include <iostream>

template <typename T> class IndexedVector {
public:
	using Index = int;
	static constexpr Index invalid_index = -1;

private:
	struct Unused {
		Index next_unused;
	};

	std::vector<duck::Variant::Static<Unused, T>> object_by_index_;
	std::unordered_map<T, Index> index_by_object_;
	Index unused_indexes_free_list_{invalid_index};

	bool free_list_is_empty () const noexcept { return unused_indexes_free_list_ == invalid_index; }
	Index pop_from_unused_free_list () {
		auto index = unused_indexes_free_list_;
		unused_indexes_free_list_ = object_by_index_.at (index).template get<Unused> ().next_unused;
		return index;
	}

	Index allocate_free_index () {
		if (!free_list_is_empty ()) {
			return pop_from_unused_free_list ();
		} else {
			auto index = static_cast<Index> (object_by_index_.size ());
			object_by_index_.emplace_back ();
			return index;
		}
	}

public:
	template <typename... Args> Index create_object (Args &&... args) {
		auto index = allocate_free_index ();
		auto & cell = object_by_index_.at (index);
		auto & obj = cell.template emplace<T> (std::forward<Args> (args)...);
		index_by_object_.emplace (obj, index);
		return index;
	}

	T & object (Index index) { return object_by_index_.at (index).template get<T> (); }
	const T & object (Index index) const { return object_by_index_.at (index).template get<T> (); }

	Index index_of (const T & t) const noexcept {
		auto it = index_by_object_.find (t);
		if (it != index_by_object_.end ()) {
			return it->second;
		} else {
			return invalid_index;
		}
	}
};

int main (int argc, char * argv[]) {
	// Qt setup
	QApplication app (argc, argv);
	QCoreApplication::setApplicationName ("rett");
	QApplication::setApplicationDisplayName ("Rett");
#define XSTR(x) #x
#define STR(x) XSTR (x)
	QCoreApplication::setApplicationVersion (STR (RETT_VERSION));
#undef STR
#undef XSTR

	for (auto i : duck::range (42))
		std::cout << i << ' ';
	std::cout << '\n';

	IndexedVector<int> iv;
	auto a = iv.create_object (42);
	std::cout << a << ' ' << iv.object (a) << '\n';

	QTimer::singleShot (0, qApp, SLOT (quit ()));
	return app.exec ();
}
