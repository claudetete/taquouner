#
#  syncContacts.py
#  Synchronize with Google Gmail Contacts
#  Based on GData Sample Codes
#
#   Developed by Cristiano Lazzari <crlazzari@gmail.com>
#   Date Nov 27, 2009
#
#
#

import sys
import getopt
import getpass
import atom
import gdata.contacts
import gdata.contacts.client
import gdata.contacts.data
import gdata.contacts.service
import string
import os
import gdata.data


## 
class ContactBBDBPhone():
  def __init__(self):
    self.location = ''
    self.number   = ''

## 
class ContactBBDBAddress():
  def __init__(self):
    self.location = ''
    self.street1  = ''
    self.street2  = ''
    self.street3  = ''
    self.zip      = ''
    self.city     = ''
    self.state    = ''
    self.country  = ''

## 
class ContactBBDBOther():
  def __init__(self):
    self.name = ''
    self.val  = ''


##
class ContactBBDB():
  def __init__(self):
    self.name     = ''
    self.title    = ''
    self.nickname = ''
    self.company  = ''
    self.phones   = []
    self.address  = []
    self.emails   = []
    self.website  = ''
    self.notes    = ''
    self.creation = ''
    self.modified = ''
    self.other    = []

##
class ContactsSync(object):
  """ContactsSync object w/ operations in the Contacts feed."""

  def __init__(self, email, password):
    """Constructor for the ContactsSample object.
    
    Takes an email and password corresponding to a gmail account to
    demonstrate the functionality of the Contacts feed.
    
    Args:
      email: [string] The e-mail address of the account to use for the sample.
      password: [string] The password corresponding to the account specified by
          the email parameter.    
    """
    self.gd_client = gdata.contacts.service.ContactsService()
    self.gd_client.email = email
    self.gd_client.password = password
    self.gd_client.source = 'syncContacts'
    self.gd_client.ProgrammaticLogin()

  def PrintFeed(self, feed):
    """Prints out the contents of a feed to the console.
   
    Args:
      feed: A gdata.contacts.ContactsFeed instance.
    """
    print '\n'
    if not feed.entry:
      print 'No entries in feed.\n'
    while feed:
      for i, entry in enumerate(feed.entry):
         print '\n%s %s' % (i, entry.title.text)
         if entry.content:
           print '    %s' % (entry.content.text)
         for email in entry.email:
           if email.primary and email.primary == 'true':
             print '    %s' % (email.address)
      next = feed.GetNextLink()
      feed = None
      if next:
        feed = self.gd_client.GetContactsFeed(next.href)
  

  ##
  def getContactIdx(self, feed, _email):
    """Find a Contact by the e-mail"""

    if not feed.entry:
      return -1

    idx = 0;
    while feed:
       for i, entry in enumerate(feed.entry):
         for email in entry.email:
           if email.address == _email:
             return idx  
         idx = idx + 1
       next = feed.GetNextLink()
       feed = None
       if next:
          feed = self.gd_client.GetContactsFeed(next.href)

    return -1


  ##
  def getContactFeedEntry(self, feed, _email):
    """Find a Contact by the e-mail"""

    if not feed.entry:
      return None

    while feed:
       for i, entry in enumerate(feed.entry):
         for email in entry.email:
           if email.address == _email:
             return entry
       next = feed.GetNextLink()
       feed = None
       if next:
          feed = self.gd_client.GetContactsFeed(next.href)

    return None


  ##
  def formatAddress(self, _addr):
     return "%s\n%s\n%s\n%s, %s\n%s, %s" % ( _addr.street1, _addr.street2, _addr.street3,
	_addr.city, _addr.state, 
	_addr.zip, _addr.country )

  ##
  def contacts2BBDB(self, feed):
    
    fname = "%s/.bbdb" % os.path.expanduser("~")

    f = open(fname, 'w')
    
    f.write(';; -*-coding: utf-8-emacs;-*-\n')
    f.write(';;; file-version: 6\n')
    f.write(';;; user-fields: (title website department)\n')

    while feed:
       for i, entry in enumerate(feed.entry):
         f.write('[')
         
         ename     = entry.title.text

         enickname = 'nil'
         if entry.nickname:
           enickname = '"%s"' % entry.nickname.text

         ecompany  = 'nil' 
         if entry.organization:
           if entry.organization.org_name and entry.organization.org_name.text:
             ecompany = '"%s"' % entry.organization.org_name.text
         
         eemails = ''
         for email in entry.email:
           eemails = '%s "%s"' % ( eemails, email.address )

         eaddress = '('
         for address in entry.postal_address: 
           adrtype = 'Other' 
           if ( address.rel == gdata.contacts.REL_WORK ):
             adrtype = 'Work'    
           elif ( address.rel == gdata.contacts.REL_HOME ):
             adrtype = 'Home'
           adr = address.text.strip().split("\n")     
           eaddress = '%s ["%s"' % ( eaddress, adrtype )
           for j,addpt in  enumerate(adr):
             if j == 0:
               eaddress = '%s ["%s"]' % ( eaddress, addpt )
             else:
               eaddress = '%s "%s"' % ( eaddress, addpt )
           eaddress = '%s ]' % eaddress
         eaddress = '%s )' % eaddress
         if eaddress == '( )':
           eaddress = 'nil'

         ephones = '('
         for phone in entry.phone_number:    
           phonetype = 'Other'      
           if ( phone.rel == gdata.contacts.PHONE_WORK ):
             phonetype = 'Work'    
           elif ( phone.rel == gdata.contacts.PHONE_HOME ):
             phonetype = 'Home'
           elif ( phone.rel == gdata.contacts.PHONE_MOBILE ):
             phonetype = 'Mobile'
           ephones = '%s ["%s" "%s"]' % ( ephones, phonetype, phone.text )
         ephones = '%s )' % ephones
         if ephones == '( )':
           ephones = 'nil'

         emore = '' #'(creation-date . "2010-01-01") (timestamp . "2010-01-01")'
         
#         if entry.website:
#           emore = '%s (website . "%s")' % ( emore, entry.website.text )

         if entry.content and entry.content.text :
           enotes = entry.content.text.strip().split("\n") 
           emore = '%s (notes . "' % emore
           for enote in enotes:
               emore = '%s %s' % ( emore, enote )
           emore = '%s")' % emore

         if entry.organization:
           if entry.organization.org_title and entry.organization.org_title.text:
             emore = '%s (title . "%s")' % ( emore, entry.organization.org_title.text )

#         for extended_property in entry.extended_property:
#           if extended_property.value:
#             value = extended_property.value
#           else:
#             value = extended_property.GetXmlBlobString()
#           emore = '%s (%s . "%s")' % ( emore, extended_property.name, value )

#         if entry.department and entry.department.text :
#           emore = '%s (department . "%s")' % ( emore, entry.department.text )

         str = '"%s" "" %s %s %s %s (%s ) (%s) nil' % ( ename, enickname, ecompany, ephones, eaddress, eemails, emore )
#         str = '"%s" "" %s %s %s %s (%s ) () nil' % ( ename, enickname, ecompany, ephones, eaddress, eemails )
         f.write( str );
         
         
         f.write(']\n')
         # end for

       next = feed.GetNextLink()
       feed = None
       if next:
          feed = self.gd_client.GetContactsFeed(next.href)


    f.close()

    return None
    ## END contacts2BBDB

  ##
  def syncContact(self, feed, ctt):
    """  """ 
    
    # cttidx = -1
    csel = None
    for _email in ctt.emails:
       csel = self.getContactFeedEntry( feed, _email )
       if csel != None:
         break

    # if cttidx > 0:
#    csel = self.getContactFeedEntry( feed, _email )
    if csel != None:
      
      print 'MSG: ', ctt.name, 'was found!!'

      return None

    else:

      return None

      print 'MSG: ', ctt.name, ' was NOT found in the Gmail database. Contact will be inserted'
      
      new_contact = gdata.contacts.ContactEntry(title=atom.Title(text=ctt.name))

#      new_contact.name = atom.Name(text=ctt.name)


      if len(ctt.notes) > 0:
         new_contact.content = atom.Content(text=ctt.notes)      
      if len(ctt.nickname) > 0:
        new_contact.nickname = gdata.contacts.Nickname(ctt.nickname)
      if len(ctt.website) > 0:
        new_contact.website.append(gdata.contacts.Website(href=ctt.website, label='Other'))

      if len(ctt.company) > 0:
        org = gdata.contacts.Organization(org_name = gdata.contacts.OrgName(ctt.company), org_title = gdata.contacts.OrgTitle(ctt.title))
        new_contact.organization = org
      
      primaryemail = True
      for _email in ctt.emails:
         if primaryemail:
           new_contact.email.append(gdata.contacts.Email(address=_email, 
              primary='true', rel=gdata.contacts.REL_OTHER))
         else:
           new_contact.email.append(gdata.contacts.Email(address=_email, 
              primary='false', rel=gdata.contacts.REL_OTHER))
         primaryemail = False

      for _addr in ctt.address:
        rel = gdata.contacts.REL_OTHER
        if _addr.location == 'Work':
          rel = gdata.contacts.REL_WORK
        elif _addr.location == 'Home':
          rel = gdata.contacts.REL_HOME
	new_contact.postal_address.append(gdata.contacts.PostalAddress(rel=rel, text=self.formatAddress(_addr)))


      for _phone in ctt.phones:
        rel = gdata.contacts.PHONE_OTHER
        if _phone.location == 'Work':
          rel = gdata.contacts.PHONE_WORK
        elif _phone.location == 'Home':
          rel = gdata.contacts.PHONE_HOME
        elif _phone.location == 'Mobile':
          rel = gdata.contacts.PHONE_MOBILE
        new_contact.phone_number.append(gdata.contacts.PhoneNumber(rel=rel, text=_phone.number))

      for _other in ctt.other:
#	new_contact.extended_property.append(gdata.ExtendedProperty(name=_other.name, value=_other.val))
	new_contact.user_defined_field.append(gdata.UserDefinedField(key=_other.name, value=_other.val))

    entry = self.gd_client.CreateContact(new_contact)

    if entry:
      print 'MSG: Creation successful!'
    else:
      print 'MSG: Upload error.'

    sys.exit(1)
    
  ##
  def Run(self):
    """Bring Contacts and generate BBDB"""    
      
    # Bring Contacts
    feed = self.gd_client.GetContactsFeed()
    
    self.contacts2BBDB( feed )

    return None

#    self.PrintFeed(feed)
  
#    sys.exit(1)	

    ctt  = ''
    addr = ''

    while True:
      input = raw_input()
      input = input[0:].split(' ')


      cmd  = input[0]
      prop = string.join(input[1:])

      propmf = prop[0:].split(' ')
      prop1  = propmf[0]
      prop2  = string.join(propmf[1:])

      print 'MSG: READ ' , cmd, ' - ', prop

      if cmd == 'BEGINCONTACT':
          ctt = ContactBBDB()
      elif cmd == 'NAME':
          ctt.name = prop
      elif cmd == 'NICKNAME':
          ctt.nickname = prop
      elif cmd == 'COMPANY':
          ctt.company = prop
      elif cmd == 'NOTES':
          ctt.notes = prop
      elif cmd == 'PHONE':
          phone = ContactBBDBPhone()
          phone.location = prop1
          phone.number   = prop2
          ctt.phones.append( phone )
      elif cmd == 'EMAIL':
          ctt.emails.append( prop )
      elif cmd == 'ADDRLOCATION':
          addr = ContactBBDBAddress()
      elif cmd == 'ADDRSTREET1':
          addr.street1 = prop
      elif cmd == 'ADDRSTREET2':
          addr.street2 = prop
      elif cmd == 'ADDRSTREET3':
          addr.street3 = prop
      elif cmd == 'ADDRZIP':
          addr.zip = prop
      elif cmd == 'ADDRCITY':
          addr.city = prop
      elif cmd == 'ADDRSTATE':
          addr.state = prop
      elif cmd == 'ADDRCOUNTRY':
          addr.country = prop
          ctt.address.append( addr )
      elif cmd == 'TITLE':
          ctt.title = prop
      elif cmd == 'WEBSITE':
          ctt.website = prop
      elif cmd == 'CREATION':
          ctt.creation = prop
      elif cmd == 'MODIFIED':
          ctt.modified = prop
      elif cmd == 'OTHER':
          oth = ContactBBDBOther()
          oth.name  = prop1
          oth.value = prop2
          ctt.other.append( oth )
      elif cmd == 'ENDCONTACT':
          self.syncContact( feed, ctt )
      elif input == 'THISISTHEEND':
          break

      ## Start the creation of BBDB file
      
      
      
##
def main():
  """Syncronizes with Google Gmail Contacts."""
 
  try:
    opts, args = getopt.getopt(sys.argv[1:], '', ['user=', 'pw='])
  except getopt.error, msg:
    print 'python syncContacts.py --user [username] --pw [password]'
    sys.exit(2)

  user = ''
  pw = ''
  # Process options
  for option, arg in opts:
    if option == '--user':
      user = arg
    elif option == '--pw':
      pw = arg


  try:
    sync = ContactsSync(user, pw)
    print 'MSG: Connection to Google OK!!!'
  except gdata.service.BadAuthentication:
    print 'Invalid user credentials given.'
    return

  sync.Run()


if __name__ == '__main__':
  main()



# END
