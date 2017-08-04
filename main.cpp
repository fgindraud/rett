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

#include <duck/range/range.h>
#include <iostream>

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

	return app.exec ();
}
