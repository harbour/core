/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for version information
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/*
 * ChangeLog:
 *
 * V 1.62   Patrick Mast                Bumped build to 31a and updated date
 * V 1.61   Patrick Mast                Changed day on build date to 27
 * V 1.6    Patrick Mast                Bumped build to 31 and updated date
 * V 1.5    David G. Holm               Bumped revision code to "b" and
 *                                      changed date for public release.
 * V 1.4    David G. Holm               Bumped revision code to "a" and
 *                                      set date for public OS/2 build.
 * V 1.3    ?
 * V 1.2    ?
 * V 1.1    ?                           Renamed to hbver.h
 * V 1.11   Patrick Mast                Bumped build to 29 and updated date
 *                                      for new public release.
 * V 1.12   David G. Holm               Bumped build to 29, reset revision
 *                                      code to an empty string, and updated
 *                                      date, anticipating a public release.
 * V 1.11   Patrick Mast                Bumped build to 28 and updated date
 *                                      for new public release.
 * V 1.10   David G. Holm               Added my email address.
 * V 1.9    David G. Holm               Added Copyright and License notice.
 *                                      Added version history.
 *                                      Bumped revision code to "d" and
 *                                      updated date.
 * V 1.8    Victor Szakats              Change not documented.
 * V 1.7    David G. Holm               Bumped revision code to "c".
 * V 1.6    Patrick Mast                Bumped build to 27 and updated date
 *                                      for new public release.
 * V 1.5    Gonzalo A. Diethelm         Added RCS Id keyword.
 * V 1.4    David G. Holm               Bumped revision code to "b".
 * V 1.3    Patrick Mast                Bumped build to 26 and updated date
 *                                      for new public release.
 * V 1.2    Patrick Mast                Change not documented.
 * V 1.1    David G. Holm               Committed to CVS.
 * V 1.0    David G. Holm               Original version.
 *
 */

#ifndef HB_VER_H_
#define HB_VER_H_

/*
extern int    hb_major;
extern int    hb_minor;
extern char * hb_revision;
extern int    hb_build;
extern int    hb_year;
extern int    hb_month;
extern int    hb_day;
*/

#define hb_major     0       /* Major version number */
#define hb_minor     0       /* Minor version number */
#define hb_revision  "b"     /* Revision letter */
#define hb_build     31      /* Build number */
#define hb_year      2000    /* Build year */
#define hb_month     03      /* Build month */
#define hb_day       02      /* Build day */

#endif /* HB_VER_H_ */
