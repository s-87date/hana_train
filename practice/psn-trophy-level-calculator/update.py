from selenium import webdriver
from selenium.webdriver.common.keys import Keys

psnID = raw_input('Hello friend. Please enter your PSN ID: ')
driver = webdriver.PhantomJS()
driver.get('http://www.psnprofiles.com')
assert 'PSNProfiles' in driver.title
elem = driver.find_element_by_id('psnId')
elem.send_keys(psnID + Keys.RETURN)
driver.quit()
print ('Your profile has been updated.')