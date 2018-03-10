from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import sys

psnID = raw_input('Hello friend. Please enter your PSN ID: ')
driver = webdriver.PhantomJS()
driver.get('http://www.psnprofiles.com/' + psnID)
driver.save_screenshot('screenshot.jpg')
if psnID.lower() not in driver.title.lower():
	print 'No existing PSN ID registered on https://www.psnprofiles.com' + '\n' + 'Now processing your account.'
	driver.get('http://www.psnprofiles.com')
	assert 'PSNProfiles' in driver.title
	elem = driver.find_element_by_id('psnId')
	elem.send_keys(psnID + Keys.RETURN)
	driver.get('http://www.psnprofiles.com/' + psnID)
	driver.save_screenshot('screenshot.jpg')
	if psnID.lower() not in driver.title.lower():
		driver.quit()
		sys.exit('You have entered an invalid PSN ID.')
bronze = int(driver.find_element_by_xpath("//td[@class='bronze']/span[@class='typo-top']").text.replace(',',''))
silver = int(driver.find_element_by_xpath("//td[@class='silver']/span[@class='typo-top']").text.replace(',',''))
gold = int(driver.find_element_by_xpath("//td[@class='gold']/span[@class='typo-top']").text.replace(',',''))
platinum = int(driver.find_element_by_xpath("//td[@class='platinum']/span[@class='typo-top']").text.replace(',',''))
driver.quit()

'''
Legacy Implementation

print('Hello friend. Please input the number of trophies you have to see how many points you are from the next level.')

bronze = input('Bronze trophies: ')
silver = input('Silver trophies: ')
gold = input('Gold trophies: ')
platinum = input('Platinum trophies: ')
'''

points = 15 * bronze + 30 * silver + 90 * gold + 180 * platinum

level_array=[0,200,600,1200,2400,4000]
i = 5
while i < 11:
	level_array.append(level_array[i] + 2000)
	i += 1
while i < 25:
	level_array.append(level_array[i] + 8000)
	i += 1
while points > level_array[-1]:
	level_array.append(level_array[i] + 10000)
	i += 1
level = 1
while points > level_array[level-1]:
	level += 1
else:
	left = level_array[level-1] - points
	points_in_level = level_array[level-1] - level_array[level-2]
	percentage = str(100 * (points_in_level - left) / points_in_level) + '%'
	print 'You are currently at level ' + str(level-1) + ' and ' + percentage + '. You need ' + str(left) + ' points until level ' + str(level) + '.'