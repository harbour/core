/*
 * $Id$
 */

#include "set.ch"

function main()
   local dDate := CTOD ("04/30/99")

   outstd (SET (_SET_DATEFORMAT), dDate, hb_eol())
   set (_SET_DATEFORMAT, "yyy/mm/ddd")
   outstd (SET (_SET_DATEFORMAT), dDate, hb_eol())
   __SETCENTURY ( "on" )
   outstd (SET (_SET_DATEFORMAT), dDate, hb_eol())
   __SETCENTURY ( "off" )
   outstd (SET (_SET_DATEFORMAT), dDate, hb_eol())
   set (_SET_DATEFORMAT, "yyy/m/d/yyy")
   outstd (SET (_SET_DATEFORMAT), dDate, hb_eol())
   __SETCENTURY ( "on" )
   outstd (SET (_SET_DATEFORMAT), dDate, hb_eol())
   __SETCENTURY ( "off" )
   outstd (SET (_SET_DATEFORMAT), dDate, hb_eol())
   set (_SET_DATEFORMAT, "m/d/y/m/d")
   outstd (SET (_SET_DATEFORMAT), dDate, hb_eol())
   __SETCENTURY ( "on" )
   outstd (SET (_SET_DATEFORMAT), dDate, hb_eol())
   __SETCENTURY ( "off" )
   outstd (SET (_SET_DATEFORMAT), dDate, hb_eol())
   set (_SET_DATEFORMAT, "mmmm/ddddd")
   outstd (SET (_SET_DATEFORMAT), dDate, hb_eol())
   __SETCENTURY ( "on" )
   outstd (SET (_SET_DATEFORMAT), dDate, hb_eol())
   __SETCENTURY ( "off" )
   outstd (SET (_SET_DATEFORMAT), dDate, hb_eol())
   set (_SET_DATEFORMAT, "mmmmm/dd")
   outstd (SET (_SET_DATEFORMAT), dDate, hb_eol())
   __SETCENTURY ( "on" )
   outstd (SET (_SET_DATEFORMAT), dDate, hb_eol())
   __SETCENTURY ( "off" )
   outstd (SET (_SET_DATEFORMAT), dDate, hb_eol())

   return nil
