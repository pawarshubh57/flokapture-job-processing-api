using System;
using System.IO;
using System.IO.Compression;
using System.Linq;
using System.Threading.Tasks;
using System.Windows.Forms;
using BusinessLayer.DbEntities;

namespace CodeVortoWinFormApplication
{
    public partial class FloKaptureProcessingAgent : Form
    {
        private readonly ClsProjectMaster _clsProjectMaster = new ClsProjectMaster();

        public FloKaptureProcessingAgent()
        {
            try
            {
                CodeVortoAgentApiProcess.StartAgentApi();
                InitializeComponent();
                FormTabControl.SelectTab("tabPage4");
                BindAllSolutions();
            }
            catch (SystemException systemException)
            {
                Console.WriteLine(systemException.Message);
                CodeVortoAgentApiProcess.StopAgentApiProcess();
            }
            catch (Exception exception)
            {
                Console.WriteLine(exception.Message);
                CodeVortoAgentApiProcess.StopAgentApiProcess();
            }
        }

        protected override void OnClosed(EventArgs e)
        {
            base.OnClosed(e);
            CodeVortoAgentApiProcess.StopAgentApiProcess();
        }

        private async void BindAllSolutions()
        {
            var lstSolutions = await _clsProjectMaster.GetAllSolutions().ContinueWith(t =>
            {
                var languages = t.Result;
                return languages;
            }).ConfigureAwait(true);

            DdlLanguage.DataSource = lstSolutions;
            DdlLanguage.DisplayMember = "SolutionType";
            DdlLanguage.ValueMember = "SolutionId";

            DdlSolutions.DataSource = lstSolutions;
            DdlSolutions.DisplayMember = "SolutionType";
            DdlSolutions.ValueMember = "SolutionId";
        }

        private async void BindAllProjects(int laguageId)
        {
            var lstProjects = await _clsProjectMaster.GetAllProjects().ContinueWith(t =>
            {
                var projects = t.Result;
                return projects.Where(l => l.LanguageId == laguageId).ToList();
            }).ConfigureAwait(true);

            DdlProjects.DataSource = lstProjects;
            DdlProjects.DisplayMember = "ProjectName";
            DdlProjects.ValueMember = "ProjectId";
        }

        private void FloKaptureProcessingAgent_FormClosing(object sender, FormClosingEventArgs e)
        {
            CodeVortoAgentApiProcess.StopAgentApiProcess();
        }

        private void BtnCancel_Click(object sender, EventArgs e)
        {
            CodeVortoAgentApiProcess.StopAgentApiProcess();
            Close();
        }

        private void DdlLanguage_SelectedValueChanged(object sender, EventArgs e)
        {
            var selectedValue = (SolutionMaster)DdlLanguage.SelectedItem;
            if (selectedValue != null)
                BindAllProjects(selectedValue.SolutionId);
        }

        private async void BtnStartProcess_Click(object sender, EventArgs e)
        {
            // Take SolutionId and ProjectId from dropdown and start processing it...
            // var solutionMaster = (SolutionMaster)DdlLanguage.SelectedItem;
            var projectMaster = (ProjectMaster)DdlProjects.SelectedItem;

            await _clsProjectMaster.StartProjectProcessing(projectMaster.ProjectId).ConfigureAwait(true);
            FormTabControl.SelectTab("tabPage3");
        }

        private void BtnBrowse_Click(object sender, EventArgs e)
        {
            var dialogResult = OpenFileDialog.ShowDialog();
            if (dialogResult == DialogResult.OK)
            {
                TxtProjectZipPath.Text = OpenFileDialog.FileName;
            }
        }

        private async Task GetAllProjectTypes(int solutionId)
        {
            var lstProjects = await _clsProjectMaster.GetAllProjectTypes(solutionId).ContinueWith(t =>
            {
                var projectTypes = t.Result;
                return projectTypes.ToList();
            }).ConfigureAwait(true);

            lstProjects.Insert(0, new ProjectType { ProjectTypeName = "Select", ProjectTypeId = 0 });
            DdlProjectTypes.DataSource = lstProjects;
            DdlProjectTypes.DisplayMember = "ProjectTypeName";
            DdlProjectTypes.ValueMember = "ProjectTypeId";
        }

        private async void DdlSolutions_SelectedIndexChanged(object sender, EventArgs e)
        {
            try
            {
                var selectedValue = (SolutionMaster)DdlSolutions.SelectedItem;
                if (selectedValue != null && selectedValue.SolutionId != 0)
                    await GetAllProjectTypes(selectedValue.SolutionId).ConfigureAwait(true);
            }
            catch (ArgumentException argumentException)
            {
                Console.WriteLine(argumentException.Message);
            }
        }

        private async void BtnUploadProject_Click(object sender, EventArgs e)
        {
            string zipFolderPath = TxtProjectZipPath.Text;
            string extractPath = @"D:\Auctor\CodeVorto\UploadedProjects\";

            try
            {
                if (!Directory.Exists(extractPath))
                    Directory.CreateDirectory(extractPath);

                using (var zipFileToOpen = new FileStream(zipFolderPath, FileMode.Open))
                using (var archive = new ZipArchive(zipFileToOpen, ZipArchiveMode.Read))
                {
                    archive.ExtractToDirectory(extractPath);
                }
                var solutionMaster = (SolutionMaster)DdlSolutions.SelectedItem;
                var projectMaster = new ProjectMaster
                {
                    ProjectId = 0,
                    LanguageId = solutionMaster.SolutionId,
                    PhysicalPath = extractPath + Path.GetFileNameWithoutExtension(zipFolderPath),
                    ProjectName = TxtProjectName.Text,
                    SolutionId = solutionMaster.SolutionId,
                    ProjectConfigType = 8,
                    Active = 1
                };
                int result = await _clsProjectMaster.UploadProject(projectMaster).ConfigureAwait(true);
                if (result == 1)
                    Console.WriteLine(@"Project uploaded successfully");
                Console.WriteLine(@"Error occured, please try later");
            }
            catch (Exception exception)
            {
                Console.WriteLine(@"Project not extracted to destination directory. Please try again." +
                                  exception.Message);
            }
        }
    }
}