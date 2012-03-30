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

  ##
  def formatAddress(self, _street, _city, _state, _zip, _country):
     return "%s\n%s\n%s\n%s\n%s" % ( _street, _city, _state, _zip, _country )

  ##
  def insertContact(self, _name, _email, _company, _title, _department, _phone, _street, _city, _state, _zip, _country, _notes ):
    """Insert a Contact"""
           
    new_contact = gdata.contacts.ContactEntry(title=atom.Title(text=_name))
    new_contact.email.append(gdata.contacts.Email(address=_email, 
       primary='true', rel=gdata.contacts.REL_WORK))

    if len(_company) > 0 or len(_title) > 0:
      org = gdata.contacts.Organization(org_name = gdata.contacts.OrgName(_company), org_title = gdata.contacts.OrgTitle(_title))
      new_contact.organization = org

    if len(_phone) > 0:
      rel = gdata.contacts.PHONE_WORK
      new_contact.phone_number.append(gdata.contacts.PhoneNumber(rel=rel, text=_phone))

      if len(_street) > 0 or len(_city) > 0 or len(_state) > 0 or len(_zip) > 0 or len(_country) > 0:
        addr = self.formatAddress( _street, _city, _state, _zip, _country )
        rel = gdata.contacts.REL_WORK
        new_contact.postal_address.append(gdata.contacts.PostalAddress(rel=rel, text=addr))

    if len(_department) > 0:
      new_contact.extended_property.append(gdata.ExtendedProperty(name='Department', value=_department))
#      new_contact.user_defined_field.append(gdata.UserDefinedField(key='Department', value=_department))

    if len(_notes) > 0:
      new_contact.content = atom.Content(text=_notes)  

    entry = self.gd_client.CreateContact(new_contact)

    if entry:
      print 'MSG: Creation successful!'
    else:
      print 'MSG: Upload error.'
      

    sys.exit(1)


#      new_contact.name = atom.Name(text=ctt.name)


#     if len(ctt.notes) > 0:
#       new_contact.content = atom.Content(text=ctt.notes)      
#     if len(ctt.nickname) > 0:
#       new_contact.nickname = gdata.contacts.Nickname(ctt.nickname)
#     if len(ctt.website) > 0:
#       new_contact.website.append(gdata.contacts.Website(href=ctt.website, label='Other'))

#     if len(ctt.company) > 0:
#       org = gdata.contacts.Organization(org_name = gdata.contacts.OrgName(ctt.company), org_title = gdata.contacts.OrgTitle(ctt.title))
#       new_contact.organization = org
      
#     primaryemail = True
#     for _email in ctt.emails:
#          if primaryemail:
#            new_contact.email.append(gdata.contacts.Email(address=_email, 
#               primary='true', rel=gdata.contacts.REL_OTHER))
#          else:
#            new_contact.email.append(gdata.contacts.Email(address=_email, 
#               primary='false', rel=gdata.contacts.REL_OTHER))
#          primaryemail = False

#       for _addr in ctt.address:
#         rel = gdata.contacts.REL_OTHER
#         if _addr.location == 'Work':
#           rel = gdata.contacts.REL_WORK
#         elif _addr.location == 'Home':
#           rel = gdata.contacts.REL_HOME
# 	new_contact.postal_address.append(gdata.contacts.PostalAddress(rel=rel, text=self.formatAddress(_addr)))


#       for _phone in ctt.phones:
#         rel = gdata.contacts.PHONE_OTHER
#         if _phone.location == 'Work':
#           rel = gdata.contacts.PHONE_WORK
#         elif _phone.location == 'Home':
#           rel = gdata.contacts.PHONE_HOME
#         elif _phone.location == 'Mobile':
#           rel = gdata.contacts.PHONE_MOBILE
#         new_contact.phone_number.append(gdata.contacts.PhoneNumber(rel=rel, text=_phone.number))

#       for _other in ctt.other:
# #	new_contact.extended_property.append(gdata.ExtendedProperty(name=_other.name, value=_other.val))
# 	new_contact.user_defined_field.append(gdata.UserDefinedField(key=_other.name, value=_other.val))

#     entry = self.gd_client.CreateContact(new_contact)

#     if entry:
#       print 'MSG: Creation successful!'
#     else:
#       print 'MSG: Upload error.'

#     sys.exit(1)
    
  ##
  def Run(self, _name, _email, _company, _title, _department, _phone, _street, _city, _state, _zip, _country, _notes):
    """Insert contacts to Google"""    
          # 
    
    self.insertContact( _name, _email, _company, _title, _department, _phone, _street, _city, _state, _zip, _country, _notes )

    return None     
      
      
##
def main():
  """Syncronizes with Google Gmail Contacts."""
 
  try:
    opts, args = getopt.getopt(sys.argv[1:], '', ['user=', 'pw=', 'name=', 'email=', 'title=', 'department=', 'phone=', 'street=', 'city=', 'state=', 'zip=', 'country=', 'company=', 'notes='])
  except getopt.error, msg:
    print 'python insertContact.py --user [username] --pw [password] --name [full name] --email [email]'
    sys.exit(2)

  user  = ''
  pw    = ''
  name  = ''
  email = ''
  company = ''
  title = ''
  department = ''
  phone = ''
  street = ''
  city = ''
  state = ''
  zipc = ''
  country = ''
  notes = ''

  # Process options
  for option, arg in opts:
    if option == '--user':
      user = arg
    elif option == '--pw':
      pw = arg
    elif option == '--name':
      name = arg
    elif option == '--email':
      email = arg
    elif option == '--title':
      title = arg
    elif option == '--department':
      department = arg
    elif option == '--phone':
      phone = arg
    elif option == '--street':
      street = arg
    elif option == '--city':
      city = arg
    elif option == '--state':
      state = arg
    elif option == '--zip':
      zipc = arg
    elif option == '--country':
      country = arg
    elif option == '--company':
      company = arg
    elif option == '--notes':
      notes = arg

  try:
    sync = ContactsSync(user, pw)
    print 'MSG: Connection to Google OK!!!'
  except gdata.service.BadAuthentication:
    print 'Invalid user credentials given.'
    return

  sync.Run( name, email, company, title, department, phone, street, city, state, zipc, country, notes )


if __name__ == '__main__':
  main()



# END
