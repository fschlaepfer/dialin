let
  region = "us-east-2";
  accessKeyId = "dialin-deploy2";

in
{ dialin =
  { resources, ... }:
  { deployment.targetEnv = "ec2";
    deployment.ec2.accessKeyId = accessKeyId;
    deployment.ec2.region = region;
    deployment.ec2.instanceType = "t2.micro";
    deployment.ec2.keyPair = resources.ec2KeyPairs.dialin-keys;
    deployment.ec2.ebsInitialRootDiskSize = 10;
  };

  resources.ec2KeyPairs.dialin-keys =
    { inherit region accessKeyId; };
}
