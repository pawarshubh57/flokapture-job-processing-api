using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace BusinessLayer.DbEntities
{
    [Table("ProjectConfigFiles")]
    public class ProjectConfigFiles : IEquatable<ProjectConfigFiles>
    {
        [Key]
        public int ConfigFileId { get; set; }
        public int LanguageId { get; protected set; }
        public string ConfigFileExtension { get; set; }
        public string ConfigFileName { get; set; }
        public string ProjectType { get; set; }

        public bool Equals(ProjectConfigFiles other)
        {
            if (ReferenceEquals(other, null)) return false;
            if (ReferenceEquals(this, other)) return true;
            return ConfigFileExtension.Equals(other.ConfigFileExtension) && ConfigFileName.Equals(other.ConfigFileName);
        }

        public override int GetHashCode()
        {
            int hashName = ConfigFileExtension == null ? 0 : ConfigFileName.GetHashCode();
            int hashCode = 0;
            if (ConfigFileExtension != null)
            {
                hashCode = ConfigFileExtension.GetHashCode();
            } 
            return hashName ^ hashCode;
        }

        public override string ToString()
        {
            return string.Format("{0}{1}", ConfigFileName, ConfigFileExtension);
        }
    }
}