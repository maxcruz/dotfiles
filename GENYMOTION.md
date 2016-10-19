## Genymotion Google Apps

* Genymotion doesn't provide Google Apps. To install Google Apps:
* Upgrade Genymotion and VirtualBox to the latest version.
* Download two zip files: 
  - [ARM Translation Installer v1.1] (http://filetrip.net/dl?4SUOrdcMRv)
  - [Google Apps] (http://opengapps.org/) for your Android version
* Open Genymotion emulator and go to home screen then drag and drop the first file Genymotion-ARM-Translation_v1.1.zip over the emulator. A dialog will appear and show as file transfer in progress, then another dialog will appear and ask that do you want to flash it on the emulator. 
* Click OK and reboot the device by running adb reboot from your terminal or command prompt.
* Drag and drop the second file gapps-*-signed.zip and repeat the same steps as above. 
* Run adb reboot again and, once rebooted, Google Apps will be in the emulator.

At this point 'Google Apps Services' will crash frequently with the following message google play services has stopped working. Open Google Play. After providing your account details, open Google Play and update your installed Google Apps. This seems to make Google Play realize you have an old Google Play Services and will ask you to update (in my case, updating Google Hangouts required a new version of Google Play Services). I've also heard that simply waiting will also prompt you to update. The 'Google Play Services' app doesn't seem to appear otherwise - you can't search for it. You should then see an offer to update Google Play Services. Once the new Google Play Services is installed you will now have stable, working access to Google Play
