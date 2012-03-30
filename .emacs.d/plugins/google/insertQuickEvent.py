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


 def _InsertQuickAddEvent(self, content="Tennis with John today 3pm-3:30pm"):
    """Creates an event with the quick_add property set to true so the content
    is processed as quick add content instead of as an event description."""
    event = gdata.calendar.CalendarEventEntry()
    event.content = atom.Content(text=content)
    event.quick_add = gdata.calendar.QuickAdd(value='true');

    new_event = self.cal_client.InsertEvent(event, 
        '/calendar/feeds/default/private/full')
    return new_event

 def Run(self, msg):
    """Execute class"""

    quick_add_event = self._InsertQuickAddEvent(msg)



def main():
  """Insert a Quick Event in the Google Calendar"""

  # parse command line options
  try:
    opts, args = getopt.getopt(sys.argv[1:], "", ["user=", "pw=", "msg="])
  except getopt.error, msg:
    print ('python insertQuickEvent.py --user [username] --pw [password] --msg [message] ')
    sys.exit(2)

  user = ''
  pw = ''
  msgtxt = ''

  # Process options
  for o, a in opts:
    if o == "--user":
      user = a
    elif o == "--pw":
      pw = a
    elif o == "--msg":
      msgtxt = a

  if user == '' or pw == '':
    print ('python insertQuickEvent.py --user [username] --pw [password] --msg [message] ')
    sys.exit(2)

  quickEvent = CalendarInsert(user, pw)
  quickEvent.Run(msgtxt)

if __name__ == '__main__':
  main()

