Haskell Powered Doorbell!
-------------------------

For some bizarre reason I thought it would be cool to have a Haskell powered doorbell that would make our phones
chime instead of a crappy box that runs out of batteries all the time.

#### Hardware I'm using:

The transmitter and receiver I'm using are the following:
[Byron BY100](http://www.amazon.co.uk/BY100-Wireless-Button-Byron-Doorbell/dp/B006H3K8AG)
[KEEDOX DVB-T Receiver](http://www.amazon.co.uk/gp/product/B009VBUYA0)

#### Also supported by:

This [Dockerfile](https://github.com/seanparsons/watcherdocker/blob/master/Dockerfile), in a separate project for no specific reason, 
does most of the setup involving the [rtl_433](https://github.com/merbanan/rtl_433) library and tools.

Currently this uses the [Pushover](https://pushover.net/) service and app for the notification to reach our phones,
potentially I might add to this so that something in the house still warns us because sometimes there is a large
delay until the notification arrives.

There's some S3 integration for uploading a picture taken with a camera, but that needs fixing currently.

#### How does it work?

All the app does is run the rtl_433 command line tool in its listen mode,
then watches the output for certain strings that match the hardware doorbell.
Once one of those is seen (modulo some deduplication detection) it then snaps a pic,
uploads that to S3 and fires off the notification to Pushover.
