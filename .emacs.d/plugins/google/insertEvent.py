#
#  Author : Cristiano Lazzari <crlazzari@gmail.com>
#  Insert a quick event
#


try:
  from xml.etree import ElementTree # for Python 2.5 users
except ImportError:
  from elementtree import ElementTree
import gdata.calendar.service
import gdata.service
import atom.service
import gdata.calendar
import atom
import getopt
import sys
import string
import time


class CalendarInsert:

 def __init__(self, email, password):
    """Creates a CalendarService and provides ClientLogin auth details to it.
    The email and password are required arguments for ClientLogin.  The 
    CalendarService automatically sets the service to be 'cl', as is 
    appropriate for calendar.  The 'source' defined below is an arbitrary 
    string, but should be used to reference your name or the name of your
    organization, the app name and version, with '-' between each of the three
    values.  The account_type is specified to authenticate either 
    Google Accounts or Google Apps accounts.  See gdata.service or 
    http://code.google.com/apis/accounts/AuthForInstalledApps.html for more
    info on ClientLogin.  NOTE: ClientLogin should only be used for installed 
    applications and not for multi-user web applications."""

    self.cal_client = gdata.calendar.service.CalendarService()
    self.cal_client.email = email
    self.cal_client.password = password
    self.cal_client.source = 'Google-Calendar_InsertEvents-1.0'
    self.cal_client.ProgrammaticLogin()


 def _InsertEvent(self, title, content, where,
    start_time=None, end_time=None, recurrence_data=None):
    """Inserts a basic event using either start_time/end_time definitions
    or gd:recurrence RFC2445 icalendar syntax.  Specifying both types of
    dates is not valid.  Note how some members of the CalendarEventEntry
    class use arrays and others do not.  Members which are allowed to occur
    more than once in the calendar or GData "kinds" specifications are stored
    as arrays.  Even for these elements, Google Calendar may limit the number
    stored to 1.  The general motto to use when working with the Calendar data
    API is that functionality not available through the GUI will not be 
    available through the API.  Please see the GData Event "kind" document:
    http://code.google.com/apis/gdata/elements.html#gdEventKind
    for more information"""
    
    event = gdata.calendar.CalendarEventEntry()
    event.title = atom.Title(text=title)
    event.content = atom.Content(text=content)
    event.where.append(gdata.calendar.Where(value_string=where))

    if recurrence_data is not None:
      # Set a recurring event
      event.recurrence = gdata.calendar.Recurrence(text=recurrence_data)
    else:
      if start_time is None:
        # Use current time for the start_time and have the event last 1 hour
        start_time = time.strftime('%Y-%m-%dT%H:%M:%S.000Z', time.gmtime())
        end_time = time.strftime('%Y-%m-%dT%H:%M:%S.000Z', 
            time.gmtime(time.time() + 3600))
        event.when.append(gdata.calendar.When(start_time=start_time, 
          end_time=end_time))
    
    new_event = self.cal_client.InsertEvent(event, 
        '/calendar/feeds/default/private/full')
    
    return new_event

 
 def _InsertSingleEvent(self, title, content, where, start_time=None, end_time=None):
    """Uses the _InsertEvent helper method to insert a single event which
    does not have any recurrence syntax specified."""

    new_event = self._InsertEvent(title, content, where, start_time, end_time, recurrence_data=None)

    return new_event

 def _InsertRecurringEvent(self, title, content, where, start_time, end_time, recurrence_data):
    """Uses the _InsertEvent helper method to insert a recurring event which
    has only RFC2445 icalendar recurrence syntax specified.  Note the use of
    carriage return/newline pairs at the end of each line in the syntax.  Even 
    when specifying times (as opposed to only dates), VTIMEZONE syntax is not
    required if you use a standard Java timezone ID.  Please see the docs for
    more information on gd:recurrence syntax:
    http://code.google.com/apis/gdata/elements.html#gdRecurrence
    """ 

    if recurrence_data is None:
      recurrence_data = ('DTSTART;VALUE=DATE:20070501\r\n'
        + 'DTEND;VALUE=DATE:20070502\r\n'
        + 'RRULE:FREQ=WEEKLY;BYDAY=Tu;UNTIL=20070904\r\n')

    new_event = self._InsertEvent(title, content, where, 
        recurrence_data=recurrence_data, start_time=start_time, end_time=end_time)
  
    return new_event



 def Run(self, title, content, where, start_time, end_time, all_day, is_recurr, r_rule ):
    """Execute class"""

    if is_recurr is 'Y':
      if all_day is 'Y':
        recurrence_data = ('DTSTART;VALUE=DATE:' + start_time + '\r\n'
          + 'DTEND;VALUE=DATE:' + end_time + '\r\n'
          + 'RRULE:' + r_rule + '\r\n')
      else:
        recurrence_data = ('DTSTART;VALUE=DATETIME:' + start_time + '\r\n'
          + 'DTEND;VALUE=DATETIME:' + end_time + '\r\n'
          + 'RRULE:' + r_rule + '\r\n')
      ree = self._InsertRecurringEvent(title, content, where, start_time, end_time, recurrence_data)
    else:
      if all_day is 'Y':
        recurrence_data = ('DTSTART;VALUE=DATE:' + start_time + '\r\n'
            + 'DTEND;VALUE=DATE:' + end_time + '\r\n')
      else:
        recurrence_data = ('DTSTART;VALUE=DATETIME:' + start_time + '\r\n'
          + 'DTEND;VALUE=DATETIME:' + end_time + '\r\n')
      ree = self._InsertRecurringEvent(title, content, where, start_time, end_time, recurrence_data)
      

def main():
  """Insert an Event in the Google Calendar"""

  # parse command line options
  try:
    opts, args = getopt.getopt(sys.argv[1:], "", ["user=", "pw=", "t=", "c=", "w=", "st=", "et=", "ad=", "r=", "rr="])
  except getopt.error, msg:
    print ('ERROR: ')
    sys.exit(2)

  user = ''
  pw = ''
  t = ''
  c = ''
  w = ''
  st = ''
  et = ''
  ad = ''
  r = ''
  rr = ''

  # Process options
  for o, a in opts:
    if o == "--user":
      user = a
    elif o == "--pw":
      pw = a
    elif o == "--t":
      t = a
    elif o == "--c":
      c = a
    elif o == "--w":
      w = a
    elif o == "--st":
      st = a
    elif o == "--et":
      et = a
    elif o == "--r":
      r = a
    elif o == "--rr":
      rr = a
    elif o == "--ad":
      ad = a

  if user == '' or pw == '':
    sys.exit(2)

  print t, " ", c, " ", w, " ", st, " ", et, " ", ad, " ", r, " ", rr

  aEvent = CalendarInsert(user, pw)
  aEvent.Run(t, c, w, st, et, ad, r, rr)

if __name__ == '__main__':
  main()

