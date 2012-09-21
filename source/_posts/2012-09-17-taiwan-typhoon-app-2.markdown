---
layout: post
title: "Taiwan Typhoon App (2)"
date: 2012-09-16 17:30
comments: true
categories: Cocoa
---

I start from a single view application with **MKMapView**. Remeber to link
`MapKit.framework` in project setting.

This is just a prototyping app, so everything will be handled in
`FCViewController`, including

* `NSURLConnection` delegate: handle request and incoming data stream.
* Read data as string, and process it with regular expression and JSON parsing.
* `MKMapView` delegate: create and draw annotations and overlays.

<!-- more -->

{% codeblock lang:objc https://github.com/dryman/Taiwan-Typhoon-iOS/blob/master/TyphoonTW/FCViewController.h FCViewController.h %}
    #import <UIKit/UIKit.h>
    #import <MapKit/MapKit.h>
    #import <CoreLocation/CoreLocation.h>

    @interface FCViewController : UIViewController <MKMapViewDelegate,CLLocationManagerDelegate, NSURLConnectionDataDelegate>
    @property (weak, nonatomic) IBOutlet MKMapView *mapView;
    @property (nonatomic, strong) CLLocationManager *clManager;
    @property (nonatomic, strong) NSURLConnection *connection;
    @property (nonatomic, strong) NSMutableData *connectionData;

    @end
{% endcodeblock %}

## NSURLConnection

I use *NSURLConnection* for networking. There are three common ways to use it:

1. Synchronous request (Not recommended.)
2. Asynchronous request using block.
3. Asynchronous request using delegate to handle respond data.

I use third method for this app. To use it, you

1. make sure your class support `NSURLConnectionDataDelegate` protocol.
2. alloc `NSURLConnection` object, set `self` as delegate and start
   immediately (or setup some configurations like assign custom queues, and
   start manually)
3. Finally, implement these methods: 
  * `- (void) connection:(NSURLConnection *)connection didReceiveResponse:(NSURLResponse *)response` 
  * `- (void) connection:(NSURLConnection *)connection didReceiveData:(NSData *)data` 
  * `- (void) connectionDidFinishLoading:(NSURLConnection *)connection`
  * `- (void) connection:(NSURLConnection *)connection didFailWithError:(NSError *)error`

{% codeblock lang:objc %}
- (void)viewDidLoad
{
    connection = [[NSURLConnection alloc] initWithRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:@"http://www.cwb.gov.tw/V7/prevent/typhoon/Data/PTA_NEW/js/datas/ty_infos.js"]] 
                                          delegate:self
                                          startImmediately:YES];
}

#pragma mark NSURLConnection delegate
- (void) connection:(NSURLConnection *)connection didReceiveResponse:(NSURLResponse *)response
{
    connectionData = [[NSMutableData alloc] init];
}
- (void) connection:(NSURLConnection *)connection didReceiveData:(NSData *)data
{
    [connectionData appendData:data];
}
- (void) connectionDidFinishLoading:(NSURLConnection *)connection
{
    // process connection data here.
}

- (void) connection:(NSURLConnection *)connection didFailWithError:(NSError *)error
{
    connectionData = nil;
}
{% endcodeblock %}

## Process data

1. Read data as string.
2. Get JSON from JavaScript using regular expression.
3. Use `NSJSONSerialization` to parse JSON
4. Parsed data is combination of `NSArray`, `NSDictionary`, `NSString`,
   `NSNumber`, and `NSNull`.

{% codeblock lang:objc %}
- (void) connectionDidFinishLoading:(NSURLConnection *)connection
{
    NSString *ty_infos = [[NSString alloc] initWithData:connectionData encoding:NSUTF8StringEncoding];
    connectionData = nil;
    NSError *err = nil;
    NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@"\\[.+?\\];" options:NSRegularExpressionDotMatchesLineSeparators error:&err];
    NSRange range_of_match = [regex rangeOfFirstMatchInString:ty_infos options:NSRegularExpressionDotMatchesLineSeparators range:NSMakeRange(0, ty_infos.length)];
    NSString *json = [ty_infos substringWithRange:NSMakeRange(range_of_match.location, range_of_match.length-1)];
    NSArray* arr = [NSJSONSerialization JSONObjectWithData:[json dataUsingEncoding:NSUTF8StringEncoding] options:0 error:nil];
    
    
    for (NSDictionary *typhoon in arr) {
        // Use autoreleasepool to release temporary objects
        @autoreleasepool {
            size_t length = [[typhoon valueForKey:@"fcst"] count] + [[typhoon valueForKey:@"best_track"] count];
            CLLocationCoordinate2D *line_points = (CLLocationCoordinate2D*) malloc(length*sizeof(CLLocationCoordinate2D));

            int i=0;
            for (NSDictionary *track in [typhoon valueForKey:@"best_track"]) {
                CGFloat lat = [[track valueForKey:@"lat"] floatValue];
                CGFloat lon = [[track valueForKey:@"lon"] floatValue];
                CLLocationCoordinate2D location = CLLocationCoordinate2DMake(lat, lon);
                line_points[i++] = location;
                MKPointAnnotation *centerPoint = [[MKPointAnnotation alloc] init];
                centerPoint.coordinate = location;
                [self.mapView addAnnotation:centerPoint];
            }
            for (NSDictionary* fcst in [typhoon valueForKey:@"fcst"]) {
                CGFloat lat = [[fcst valueForKey:@"lat"]  floatValue];
                CGFloat lon = [[fcst valueForKey:@"lon"]  floatValue];
                CGFloat rad = [[fcst valueForKey:@"pr70"] floatValue];
                CLLocationCoordinate2D location = CLLocationCoordinate2DMake(lat, lon);
                line_points[i++]=location;
                
                MKCircle *cir = [MKCircle circleWithCenterCoordinate: location
                                                              radius: rad*1000];
                [self.mapView addOverlay:cir];
                MKPointAnnotation *centerPoint = [[MKPointAnnotation alloc] init];
                centerPoint.coordinate = location;
                [self.mapView addAnnotation:centerPoint];
            }

            MKPolyline *line = [MKPolyline polylineWithCoordinates:line_points count:length];
            [self.mapView addOverlay:line];
            free(line_points);
        }
    }
}
{% endcodeblock %}

## MapKit

MapKit is simular to most UI elements. You setup a MKMapView, add data
(MKAnnotation, MKOverlay, ...etc.) to it, and handle drawing delegate methods.
This is a prototype app, so I only use built-in `MKPointAnnotation` and
`MKPolyline` overlay. If you want to assign custom properties to annotation or
overlays, you can subclass them and implement drawing functions in delegate.

{% codeblock lang:objc %}
#pragma mark Map View Delegate methods
- (MKOverlayView*)mapView:(MKMapView *)mapView viewForOverlay:(id<MKOverlay>)overlay
{
    if ([overlay isKindOfClass:[MKCircle class]]) {
        MKCircleView *view = [[MKCircleView alloc] initWithCircle:overlay];
        view.fillColor = [[UIColor redColor] colorWithAlphaComponent:0.02];
        view.strokeColor = [[UIColor redColor] colorWithAlphaComponent:0.15];
        view.lineWidth = 2;
        return view;
    } else if ([overlay isKindOfClass:[MKPolyline class]]) {
        MKPolylineView *view = [[MKPolylineView alloc] initWithPolyline:overlay];
        view.strokeColor = [[UIColor blackColor] colorWithAlphaComponent:0.2];
        view.lineWidth = 2;
        return view;
    }
    return nil;
}
- (MKAnnotationView*)mapView:(MKMapView *)mView viewForAnnotation:(id<MKAnnotation>)annotation
{
    NSString *identifier = @"pinView";
    MKAnnotationView *annotationView = (MKAnnotationView*)[mView dequeueReusableAnnotationViewWithIdentifier:identifier];
    if (annotationView==nil) {
        annotationView = [[MKAnnotationView alloc] initWithAnnotation:annotation reuseIdentifier:identifier];
        UIImage *img = [UIImage imageNamed:@"typh.png"];
        annotationView.image = img;
        annotationView.canShowCallout = NO;
    }
    return annotationView;
}
{% endcodeblock %}

That's it. Now we have a native interface to show typhoon prediction!

<img width="50%" style="margin-left:25%;" src="/images/typhoon_tw/typhoon-09-16.png"/>

## Source code

The source code is on [github](https://github.com/dryman/Taiwan-Typhoon-iOS).
