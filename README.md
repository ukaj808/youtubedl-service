# youtubedl-service

The youtubedl program, as a service.

1) Subscribe to the predefined topic
2) Send a post request with the URL you wish to download, store the id
3) When download result event comes in for that ID, GET the download
4) DELETE the download if you don't need it anymore

curl -H "Accept: text/plain" -H "Content-type: application/json" -X POST -d '{"url":"https://www.youtube.com/watch?v=KZVqy3PZimY"}' http://[HOST]/api/v1

*202* 068ab254-dcea-40f1-b1b6-34c7b5a4d8f3

curl -H "Accept: audio/mpeg" -X GET --output ./song.mp3 http://[HOST]/api/v1/068ab254-dcea-40f1-b1b6-34c7b5a4d8f3

*200* mp3 file

curl -X DELETE http://localhost:8003/api/v1/068ab254-dcea-40f1-b1b6-34c7b5a4d8f3

*204* "Delete Successful"



## License
[MIT](https://choosealicense.com/licenses/mit/)
