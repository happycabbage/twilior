# twilior

Use the Twilio API with R! A bit more feature rich than `twilio` and on-going development that reflects real life business use of Twilio for data science. As we add more features from Twilio into our work we will keep this package updated and publicly available.

## Installation

Install using `devtools`

``` r
devtools::install_github("happycabbage/twilior")
```

To use you will need to setup an account on [Twilio](twilio.com) and get your Twilio  `ACCOUNT_ID` and `SID_ID` which are needed universally. You can add them to your `.RProfile` or the `config` package if needed to set them as enviornment variables.

```r
Sys.setenv(TWILIO_TOKEN = 'YOUR_ACCOUNT_ID')
Sys.setenv(TWILIO_SID   = 'YOUR_SID')
```

You can also set the ID if the Notify Service and Messaging Service you use if you use them.

```r
Sys.setenv(TWILIO_NOTIFY_SID = 'YOUR_NOTIFY_SID`)
Sys.setenv(TWILIO_MESSAGING_SERVICE = 'YOUR_MESSAGING_SERVICE_SID')
```

Don't like enviornment variables? Don't worry! You can always pass the params into the function directly (e.g. if you are using the `config` package).


## Usage

The functions are pretty basic if you are familiar with the Twilio API. Even if you aren't, as long as you've ever sent an SMS you should get the hang of it quick.

### Send a message

```r
tw_send_text(to = '2128675309', from = '1-800-BANANAS', message = 'Snape kills Dumbledore')
```

### Send a message using Messaging Service (do this!)
```r
tw_send_text(to = '2128675309', from = '1-800-BANANAS', message = 'Snape kills Dumbledore', service_sid = 'XX123XXXX')
```

### Get the phone numbers associated with a Messaging Service

### Download your messages

### Send a notifcation via Notify


