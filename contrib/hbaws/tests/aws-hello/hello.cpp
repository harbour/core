#include <aws/core/Aws.h>
#include <aws/core/auth/AWSCredentials.h>
#include <aws/s3/S3Client.h>
#include <aws/s3/model/ListBucketsRequest.h>
#include <aws/s3/model/ListBucketsResult.h>
#include <iostream>
#include <chrono>

int main() {
  Aws::SDKOptions options;

  Aws::InitAPI(options);
  {
    Aws::String access_key_id = "XXXXXXXXXXXXXXXXXXXXX";
    Aws::String secret_access_key = "XXXXXXXXXXXXXXXXXXXXXXXXXX";
    Aws::Auth::AWSCredentials credentials(access_key_id, secret_access_key);

    auto start = std::chrono::high_resolution_clock::now();
    std::cout << "Hello HBAWS World!" << std::endl;
    std::cout << "Waiting for AWS connection..." << std::endl;
    Aws::S3::S3Client s3_client(credentials);
    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = end - start;
    std::cout << "AWS connection done " << elapsed.count() << " seconds" << std::endl;

    for (int i = 0; i < 1; ++i)
    {
        auto outcome = s3_client.ListBuckets();

        if (outcome.IsSuccess()) {
          std::cout << "Buckets:\n";
          for (const auto &bucket : outcome.GetResult().GetBuckets()) {
            std::cout << "  * " << bucket.GetName() << std::endl;
          }
        } else {
          std::cerr << "Error: " << outcome.GetError().GetMessage() << std::endl;
        }
    }
  }
  Aws::ShutdownAPI(options);
  return 0;
}

