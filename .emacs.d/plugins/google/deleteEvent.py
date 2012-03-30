#
#  Author : Cristiano Lazzari <crlazzari@gmail.com>
#  Delete a  event
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

 ##
 def Run(self, uid):
    """Execute class"""

    query = gdata.calendar.service.CalendarEventQuery('default', 'private', 'full')
    feed = self.cal_client.CalendarQuery(query)
    for i, an_event in zip(xrange(len(feed.entry)), feed.entry):
      linkl = an_event.GetEditLink().href
      linkl = linkl.split('/')
      found = False
      for l in linkl:
         print(l)
         if l == uid:
           found = True
           break
      if found:
	self.cal_client.DeleteCalendarEntry(an_event.GetEditLink().href)



def main():
  """Delete a event in the Google Calendar"""

  # parse command line options
  try:
    opts, args = getopt.getopt(sys.argv[1:], "", ["user=", "pw=", "event="])
  except getopt.error, msg:
    print ('python deleteEvent.py --user [username] --pw [password] --event [event uid] ')
    sys.exit(2)

  user = ''
  pw = ''
  uid = ''

  # Process options
  for o, a in opts:
    if o == "--user":
      user = a
    elif o == "--pw":
      pw = a
    elif o == "--event":
      uid = a

  if user == '' or pw == '':
    print ('python insertQuickEvent.py --user [username] --pw [password] --event [event uid] ')
    sys.exit(2)

  quickEvent = CalendarInsert(user, pw)
  quickEvent.Run(uid)

if __name__ == '__main__':
  main()

